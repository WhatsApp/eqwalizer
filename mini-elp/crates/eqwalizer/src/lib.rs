/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use anyhow::Context;
use anyhow::Result;
use fxhash::FxHashMap;
use std::convert::TryInto;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::Write;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::os::unix::prelude::PermissionsExt;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::sync::Arc;
use text_size::TextRange;

use serde::Deserialize;

use tempfile::Builder;
use tempfile::TempPath;

mod ipc;
use ipc::IpcHandle;
use ipc::MsgFromEqWAlizer;
use ipc::MsgToEqWAlizer;

// Bundle file with command to make sure it's not removed too early
#[derive(Clone)]
pub struct Eqwalizer {
    cmd: OsString,
    args: Vec<OsString>,
    // Used only for the Drop implementation
    _file: Option<Arc<TempPath>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EqwalizerDiagnostics {
    Diagnostics(FxHashMap<String, Vec<EqwalizerDiagnostic>>),
    NoAst { module: String },
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct EqwalizerDiagnostic {
    #[serde(deserialize_with = "deserialize_text_range")]
    pub range: TextRange,
    pub message: String,
    pub uri: String,
    pub code: String,
    #[serde(rename(deserialize = "expressionOrNull"))]
    pub expression: Option<String>,
    #[serde(rename(deserialize = "explanationOrNull"))]
    pub explanation: Option<String>,
}

pub trait DbApi {
    fn unwind_if_cancelled(&self) -> ();
    fn get_ast(&self, module: &str) -> Option<Arc<Vec<u8>>>;
}

fn deserialize_text_range<'de, D>(deserializer: D) -> Result<TextRange, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct RawTextRange {
        start: u32,
        end: u32,
    }

    let range = RawTextRange::deserialize(deserializer)?;
    Ok(TextRange::new(range.start.into(), range.end.into()))
}

impl Default for Eqwalizer {
    fn default() -> Self {
        let env = env::var("ELP_EQWALIZER_PATH");
        let (path, ext, temp_file) = if let Ok(path) = env {
            let path = PathBuf::from(path);
            let ext = path
                .extension()
                .unwrap_or_default()
                .to_str()
                .unwrap()
                .to_string();
            (path, ext, None)
        } else {
            let extension = env!("ELP_EQWALIZER_EXT").to_string();
            let eqwalizer_src = include_bytes!(concat!(env!("OUT_DIR"), "/eqwalizer"));
            let mut temp_file = Builder::new()
                .prefix("eqwalizer")
                .tempfile()
                .expect("can't create eqwalizer temp executable");
            temp_file
                .write_all(eqwalizer_src)
                .expect("can't create eqwalizer temp executable");

            let temp_file = temp_file.into_temp_path();

            let mut perm = fs::metadata(&temp_file)
                .expect("can't create eqwalizer temp executable")
                .permissions();
            perm.set_mode(0o755);
            fs::set_permissions(&temp_file, perm).expect("can't create eqwalizer temp executable");

            (temp_file.to_path_buf(), extension, Some(temp_file))
        };

        let (cmd, args) = match ext.as_str() {
            "jar" => (
                "java".into(),
                vec!["-Xss10M".into(), "-jar".into(), path.into()],
            ),
            "" => (path.into(), vec![]),
            _ => panic!("Unknown eqwalizer executable {:?}", path),
        };

        Self {
            cmd,
            args,
            _file: temp_file.map(Arc::new),
        }
    }
}

impl Eqwalizer {
    // Return a smart pointer to bundle lifetime with the temp file's lifetime
    pub fn cmd<'file>(&'file self) -> CommandProxy<'file> {
        let mut cmd = Command::new(&self.cmd);
        cmd.args(&self.args);
        CommandProxy::new(cmd)
    }

    pub fn typecheck(
        &self,
        build_info_path: &Path,
        db_api: impl DbApi,
        modules: Vec<&str>,
    ) -> Result<EqwalizerDiagnostics> {
        let mut cmd = self.cmd();
        cmd.arg("ipc");
        cmd.args(modules);
        cmd.env("EQWALIZER_IPC", "true");
        add_env(&mut cmd, build_info_path, None);

        let handle = &mut IpcHandle::from_command(&mut cmd)?;

        loop {
            db_api.unwind_if_cancelled();
            match handle.receive()? {
                MsgFromEqWAlizer::GetAstBytes { module } => {
                    log::debug!("received from eqwalizer: GetAstBytes for module {}", module);
                    match db_api.get_ast(&module) {
                        Some(ast_bytes) => {
                            log::debug!(
                                "sending to eqwalizer: GetAstBytesReply for module {}",
                                module
                            );
                            let ast_bytes_len = ast_bytes.len().try_into()?;
                            let reply = &MsgToEqWAlizer::GetAstBytesReply { ast_bytes_len };
                            handle.send(reply)?;
                            handle.receive_newline()?;
                            handle.send_bytes(&ast_bytes)?;
                        }
                        None => {
                            log::debug!(
                                "sending to eqwalizer: CannotCompleteRequest for module {}",
                                module
                            );
                            let reply = &MsgToEqWAlizer::CannotCompleteRequest;
                            handle.send(reply)?;
                            return Ok(EqwalizerDiagnostics::NoAst { module });
                        }
                    }
                }
                MsgFromEqWAlizer::Done { diagnostics } => {
                    log::debug!(
                        "received from eqwalizer: Done with diagnostics length {}",
                        diagnostics.len()
                    );
                    return Ok(EqwalizerDiagnostics::Diagnostics(diagnostics));
                }
            }
        }
    }

    pub fn passthrough(
        &self,
        args: &[String],
        build_info_path: &Path,
        elp_ast_dir: &Path,
    ) -> Result<ExitStatus> {
        let mut cmd = self.cmd();
        cmd.args(args);
        add_env(&mut cmd, build_info_path, Some(elp_ast_dir));
        cmd.status()
            .with_context(|| "Error in eqwalizer passthrough")
    }
}

fn add_env(cmd: &mut Command, build_info_path: &Path, elp_ast_dir: Option<&Path>) {
    cmd.env("EQWALIZER_BUILD_INFO", build_info_path);
    if let Some(elp_ast_dir) = elp_ast_dir {
        cmd.env("EQWALIZER_ELP_AST_DIR", elp_ast_dir);
    }
}

/// This ensures the enclosed Command struct won't outlive the related temp file
pub struct CommandProxy<'file>(Command, PhantomData<&'file TempPath>);

impl Default for EqwalizerDiagnostics {
    fn default() -> Self {
        EqwalizerDiagnostics::Diagnostics(Default::default())
    }
}

impl<'file> CommandProxy<'file> {
    pub fn new(cmd: Command) -> Self {
        Self(cmd, PhantomData)
    }
}

impl<'file> Deref for CommandProxy<'file> {
    type Target = Command;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'file> DerefMut for CommandProxy<'file> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
