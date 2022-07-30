/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::iter;
use std::path::PathBuf;
use std::process::Child;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex;

use anyhow::anyhow;
use anyhow::Error;
use anyhow::Result;
use crossbeam_channel::bounded;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use eetf::pattern;
use fxhash::FxHashMap;
use jod_thread::JoinHandle;
use stdx::JodChild;
use tempfile::Builder;
use tempfile::TempPath;
use text_size::TextRange;

// Location information of a warning/error may come in different flavors:
// * Eqwalizer: byte offset for range.
// * Default parser: line, column for start.
// Note: Using byte offset everywhere would complicate the code,
//       since conversion requires access to source file and its encoding.

/// Start location, as returned by erl parser (see erl_anno:location()).
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StartLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Location {
    TextRange(TextRange),
    StartLocation(StartLocation),
}

#[derive(Debug)]
struct SharedState {
    _file_for_drop: TempPath,
    _child_for_drop: JodChild,
    _writer_for_drop: JoinHandle,
    _reader_for_drop: JoinHandle,
}

#[derive(Clone, Debug)]
pub struct Connection {
    sender: Sender<Request>,
    _for_drop: Arc<SharedState>,
}

#[derive(Debug, Clone)]
pub enum CompileOption {
    Includes(Vec<PathBuf>),
    Macros(Vec<eetf::Term>),
    ParseTransforms(Vec<eetf::Term>),
    ElpMetadata(eetf::Term),
}

impl Into<eetf::Term> for CompileOption {
    fn into(self) -> eetf::Term {
        match self {
            CompileOption::Includes(includes) => {
                let paths = eetf::List::from(
                    includes
                        .into_iter()
                        .map(|path| path_into_list(path).into())
                        .collect::<Vec<_>>(),
                );
                eetf::Tuple::from(vec![eetf::Atom::from("includes").into(), paths.into()]).into()
            }
            CompileOption::Macros(macros) => {
                let macros = eetf::List::from(macros);
                eetf::Tuple::from(vec![eetf::Atom::from("macros").into(), macros.into()]).into()
            }
            CompileOption::ParseTransforms(transforms) => {
                let transforms = eetf::List::from(transforms);
                let parse_transforms = eetf::Atom::from("parse_transforms");
                eetf::Tuple::from(vec![parse_transforms.into(), transforms.into()]).into()
            }
            CompileOption::ElpMetadata(elp_metadata) => {
                let label = eetf::Atom::from("elp_metadata");
                eetf::Tuple::from(vec![label.into(), elp_metadata]).into()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Format {
    Etf,
    OffsetEtf,
    Text,
}

#[derive(Debug, Clone)]
pub struct ParseRequest {
    pub options: Vec<CompileOption>,
    pub path: PathBuf,
    pub format: Format,
}

#[derive(Debug, Clone)]
enum Request {
    ParseRequest(ParseRequest, Sender<Response>),
    AddCodePath(Vec<PathBuf>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError {
    pub path: PathBuf,
    pub location: Option<Location>,
    pub msg: String,
}

#[derive(Debug)]
enum Response {
    Ok(Vec<u8>),
    Err(Vec<u8>),
    Exception(Error),
}

impl Response {
    /// Unwraps Response into a Result panicking if the response was an Exception
    pub fn decode(self, format: Format) -> Result<Vec<u8>, Vec<ParseError>> {
        match self {
            Response::Ok(data) => Ok(data),
            Response::Err(data) => match decode_errors(&data, format) {
                Ok(errors) => Err(errors),
                Err(error) => panic!("parse_server crashed {:?}", error),
            },
            Response::Exception(exception) => panic!("parse_server crashed {:?}", exception),
        }
    }
}

impl Connection {
    pub fn start() -> Result<Connection> {
        let escript_src = include_bytes!(concat!(env!("OUT_DIR"), "/parse_server/parse_server"));
        let mut escript = Builder::new().prefix("parse_server").tempfile()?;
        escript.write_all(escript_src)?;

        let mut cmd = Command::new("escript");
        cmd.arg(escript.path());

        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut proc = cmd.spawn()?;
        let escript = escript.into_temp_path();

        let (sender, writer, reader) = stdio_transport(&mut proc);

        Ok(Connection {
            sender,
            _for_drop: Arc::new(SharedState {
                _file_for_drop: escript,
                _child_for_drop: JodChild(proc),
                _writer_for_drop: writer,
                _reader_for_drop: reader,
            }),
        })
    }

    pub fn request(&self, request: ParseRequest) -> Result<Vec<u8>, Vec<ParseError>> {
        let (sender, receiver) = bounded::<Response>(0);
        let format = request.format;
        let request = Request::ParseRequest(request, sender);
        self.sender.send(request).unwrap();
        let response = receiver.recv().unwrap();
        response.decode(format)
    }

    pub fn add_code_path(&self, paths: Vec<PathBuf>) {
        let request = Request::AddCodePath(paths);
        self.sender.send(request).unwrap();
    }
}

fn stdio_transport(proc: &mut Child) -> (Sender<Request>, JoinHandle, JoinHandle) {
    let instream = BufWriter::new(proc.stdin.take().unwrap());
    let outstream = BufReader::new(proc.stdout.take().unwrap());

    let inflight = Arc::new(Mutex::new(FxHashMap::default()));

    let (writer_sender, writer_receiver) = bounded::<Request>(0);
    let writer = jod_thread::spawn({
        let inflight = inflight.clone();
        move || match writer_run(writer_receiver, instream, inflight) {
            Ok(()) => {}
            Err(err) => log::error!("writer failed with {}", err),
        }
    });

    let reader = jod_thread::spawn({
        move || match reader_run(outstream, inflight) {
            Ok(()) => {}
            Err(err) => log::error!("reader failed with {}", err),
        }
    });

    (writer_sender, writer, reader)
}

fn reader_run(
    mut outstream: BufReader<ChildStdout>,
    inflight: Arc<Mutex<FxHashMap<usize, Sender<Response>>>>,
) -> Result<()> {
    let mut line_buf = String::new();
    loop {
        line_buf.clear();
        outstream.read_line(&mut line_buf)?;
        let parts = line_buf.split_ascii_whitespace().collect::<Vec<_>>();
        if parts.len() == 0 {
            break;
        }

        let id: usize = parts[1].parse()?;
        let size: usize = parts[2].parse()?;

        let sender = inflight.lock().unwrap().remove(&id).unwrap();

        match parts[0] {
            "OK" => {
                let mut buf = vec![0; size];
                outstream.read_exact(&mut buf)?;
                sender.send(Response::Ok(buf)).unwrap();
            }
            "ERROR" => {
                let mut buf = vec![0; size];
                outstream.read_exact(&mut buf)?;
                sender.send(Response::Err(buf)).unwrap();
            }
            "EXCEPTION" => {
                let mut buf = vec![0; size];
                outstream.read_exact(&mut buf)?;
                let resp = String::from_utf8(buf).unwrap();
                let error = anyhow!("{}", resp);
                sender.send(Response::Exception(error)).unwrap();
            }
            _ => {
                log::error!("Unrecognised message: {}", line_buf);
                break;
            }
        }
    }
    Ok(())
}

fn writer_run(
    receiver: Receiver<Request>,
    mut instream: BufWriter<ChildStdin>,
    inflight: Arc<Mutex<FxHashMap<usize, Sender<Response>>>>,
) -> Result<()> {
    let mut counter = 0;
    receiver.into_iter().try_for_each(|request| match request {
        Request::ParseRequest(request, sender) => {
            counter += 1;
            inflight.lock().unwrap().insert(counter, sender);
            let tag = request.tag();
            let bytes = request.encode(counter);
            write!(instream, "{} {}\n", tag, bytes.len())?;
            instream.write_all(&bytes)?;
            instream.flush()
        }
        Request::AddCodePath(paths) => {
            write!(instream, "ADD_PATHS {}\n", paths.len())?;
            for path in paths {
                write!(instream, "{}\n", path.display())?;
            }
            Ok(())
        }
    })?;
    instream.write_all(b"EXIT\n")?;
    Ok(())
}

fn decode_errors(buf: &[u8], format: Format) -> Result<Vec<ParseError>> {
    eetf::Term::decode(buf)?
        .as_match(pattern::VarList((
            pattern::Str(pattern::Unicode),
            pattern::Or(((pattern::U32, pattern::U32), "none")),
            pattern::Str(pattern::Unicode),
        )))
        .map_err(|err| anyhow!("Failed to decode errors: {:?}", err))
        .map(|res| {
            res.into_iter()
                .map(|(path, position, msg)| ParseError {
                    path: path.into(),
                    location: position.into_result().ok().map(|(a, b)| -> Location {
                        if format.is_offset() {
                            Location::TextRange(TextRange::new(a.into(), b.into()))
                        } else {
                            Location::StartLocation(StartLocation { line: a, column: b })
                        }
                    }),
                    msg,
                })
                .collect()
        })
}

impl Format {
    pub fn is_offset(&self) -> bool {
        match self {
            Format::Etf => false,                            // Line, comment
            Format::OffsetEtf { .. } | Format::Text => true, // StartByte, EndByte
        }
    }
}

impl ParseRequest {
    fn tag(&self) -> &'static str {
        match self.format {
            Format::Etf | Format::OffsetEtf { .. } => "COMPILE",
            Format::Text => "TEXT",
        }
    }

    fn encode(self, id: usize) -> Vec<u8> {
        let location = match self.format {
            Format::Etf => {
                let one: eetf::Term = eetf::FixInteger::from(1).into();
                eetf::Tuple::from(vec![one.clone(), one]).into()
            }
            Format::OffsetEtf { .. } | Format::Text => eetf::Atom::from("offset").into(),
        };
        let location_tuple =
            eetf::Tuple::from(vec![eetf::Atom::from("location").into(), location]).into();
        let options = self
            .options
            .into_iter()
            .map(|option| option.into())
            .chain(iter::once(location_tuple))
            .collect::<Vec<eetf::Term>>();
        let tuple = eetf::Tuple::from(vec![
            eetf::BigInteger::from(id).into(),
            path_into_list(self.path).into(),
            eetf::List::from(options).into(),
        ]);
        let mut buf = Vec::new();
        eetf::Term::from(tuple).encode(&mut buf).unwrap();
        buf
    }
}

#[cfg(unix)]
fn path_into_list(path: PathBuf) -> eetf::List {
    use std::os::unix::prelude::OsStringExt;
    path.into_os_string()
        .into_vec()
        .into_iter()
        .map(|byte| eetf::FixInteger::from(byte).into())
        .collect::<Vec<eetf::Term>>()
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect_file;
    use expect_test::ExpectFile;
    use lazy_static::lazy_static;
    use std::str;

    #[test]
    fn regular_module() {
        expect_module(
            "fixtures/regular.erl".into(),
            expect_file!["../fixtures/regular.expected"],
            vec![],
        );
    }

    #[test]
    fn structured_comment() {
        expect_module(
            "fixtures/structured_comment.erl".into(),
            expect_file!["../fixtures/structured_comment.expected"],
            vec![],
        );
    }

    #[test]
    fn errors() {
        expect_module(
            "fixtures/error.erl".into(),
            expect_file!["../fixtures/error.expected"],
            vec![],
        );

        expect_module(
            "fixtures/misplaced_comment_error.erl".into(),
            expect_file!["../fixtures/misplaced_comment_error.expected"],
            vec![],
        );
    }

    #[test]
    fn warnings() {
        expect_module(
            "fixtures/error_attr.erl".into(),
            expect_file!["../fixtures/error_attr.expected"],
            vec![],
        );
    }

    fn expect_module(path: PathBuf, expected: ExpectFile, options: Vec<CompileOption>) {
        lazy_static! {
            static ref CONN: Connection = Connection::start().unwrap();
        }
        let request = ParseRequest {
            options,
            path,
            format: Format::Text,
        };
        match CONN.request(request) {
            Ok(resp) => {
                let actual = str::from_utf8(&resp).unwrap();
                expected.assert_eq(actual);
            }
            Err(errs) => expected.assert_debug_eq(&errs),
        };
    }
}
