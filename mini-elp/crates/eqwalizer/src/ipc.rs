/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;
use stdx::JodChild;

use anyhow::Context;
use anyhow::Result;
use fxhash::FxHashMap;
use serde::Deserialize;
use serde::Serialize;
use timeout_readwrite::TimeoutReader;
use timeout_readwrite::TimeoutWriter;

use crate::EqwalizerDiagnostic;

#[derive(Deserialize, Debug)]
#[serde(tag = "tag", content = "content")]
pub enum MsgFromEqWAlizer {
    GetAstBytes {
        module: String,
    },
    Done {
        diagnostics: FxHashMap<String, Vec<EqwalizerDiagnostic>>,
    },
}

#[derive(Serialize, Debug)]
#[serde(tag = "tag", content = "content")]
pub enum MsgToEqWAlizer {
    GetAstBytesReply { ast_bytes_len: u32 },
    CannotCompleteRequest,
}

pub struct IpcHandle {
    writer: BufWriter<TimeoutWriter<ChildStdin>>,
    reader: BufReader<TimeoutReader<ChildStdout>>,
    _child_for_drop: JodChild,
}

impl IpcHandle {
    pub fn from_command(cmd: &mut Command) -> Result<Self> {
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            // for debugging purposes
            .stderr(Stdio::inherit());

        let mut child = cmd.spawn()?;
        let stdin = child
            .stdin
            .take()
            .context("failed to get stdin for eqwalizer process")?;
        let stdout = child
            .stdout
            .take()
            .context("failed to get stdout for eqwalizer process")?;

        let _child_for_drop = JodChild(child);
        let writer = BufWriter::new(TimeoutWriter::new(stdin, Duration::from_secs(1)));
        let reader = BufReader::new(TimeoutReader::new(stdout, Duration::from_secs(20)));
        Ok(Self {
            writer,
            reader,
            _child_for_drop,
        })
    }

    pub fn receive(&mut self) -> Result<MsgFromEqWAlizer> {
        let buf = self.receive_line()?;
        let deserialized =
            serde_json::from_str(&buf).expect("failed to parse stdout from eqwalizer");
        Ok(deserialized)
    }

    pub fn receive_newline(&mut self) -> Result<()> {
        let _ = self.receive_line()?;
        Ok(())
    }

    pub fn send(&mut self, msg: &MsgToEqWAlizer) -> Result<()> {
        let msg = serde_json::to_string(msg).expect("failed to serialize msg to eqwalizer");
        writeln!(self.writer, "{}", msg)?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn send_bytes(&mut self, msg: &[u8]) -> Result<()> {
        // Don't exceed pipe buffer size on Mac or Linux
        // https://unix.stackexchange.com/a/11954/147568
        let chunk_size = 6_5536;
        for chunk in msg.chunks(chunk_size) {
            self.writer.write_all(chunk)?;
        }
        self.writer.flush()?;
        Ok(())
    }

    fn receive_line(&mut self) -> Result<String> {
        let mut buf = String::new();
        self.reader
            .read_line(&mut buf)
            .context("failed read_line from eqwalizer stdout")?;
        Ok(buf)
    }
}
