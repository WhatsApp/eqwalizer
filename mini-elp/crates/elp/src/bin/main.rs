/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::env;
use std::ffi::OsString;
use std::process;

use anyhow::Result;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;
use codespan_reporting::term::termcolor::WriteColor;

mod args;
mod eqwalizer_cli;
mod load_rebar;
mod parse_server_cli;
mod reporting;
mod util;

// Use jemalloc as the global allocator
#[cfg(not(target_env = "msvc"))]
use jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

fn main() {
    let out = &mut StandardStream::stdout(ColorChoice::Always);
    let err = &mut std::io::stderr();
    let mut os_args: Vec<_> = std::env::args_os().collect();
    os_args.remove(0);
    let res = try_main(out, err, os_args);
    let code = handle_res(res, err);
    process::exit(code);
}

fn handle_res(result: Result<()>, stderr: &mut impl std::io::Write) -> i32 {
    if let Err(err) = result {
        writeln!(stderr, "{:#}", err).unwrap();
        101
    } else {
        0
    }
}

fn try_main(
    out: &mut impl WriteColor,
    err: &mut impl std::io::Write,
    os_args: Vec<OsString>,
) -> Result<()> {
    let args = args::Args::parse(os_args)?;

    match args.command {
        args::Command::ParseAll(args) => parse_server_cli::parse_all(&args, out)?,
        args::Command::Eqwalize(args) => eqwalizer_cli::eqwalize_module(&args, out)?,
        args::Command::EqwalizeAll(args) => eqwalizer_cli::eqwalize_all(&args, out)?,
        args::Command::Version => {
            writeln!(out, "elp {}", env!("CARGO_PKG_VERSION"))?;
        }
        args::Command::Help => {
            writeln!(err, "{}", args::HELP)?;
        }
    }

    log::logger().flush();

    Ok(())
}

// To run the tests
// cargo test --package elp --bin elp
#[cfg(test)]
mod tests {
    use super::*;
    use codespan_reporting::term::termcolor::Buffer;
    use expect_test::expect_file;
    use expect_test::ExpectFile;
    use std::path::PathBuf;
    use std::str;
    use tempfile::Builder;

    macro_rules! args_vec {
        ($($e:expr$(,)?)+) => {
            vec![$(OsString::from($e),)+]
        }
    }

    fn elp(args: Vec<OsString>) -> (String, String, i32) {
        let mut stdout = Buffer::no_color();
        let mut stderr = Vec::new();
        let res = try_main(&mut stdout, &mut stderr, args);
        let code = handle_res(res, &mut stderr);
        let stdout = String::from_utf8(stdout.into_inner()).unwrap();
        let stderr = String::from_utf8(stderr).unwrap();
        (stdout, stderr, code)
    }

    #[test]
    fn etf_files_from_dummy_project_are_generated() {
        // Create tmp dir for output, typically /tmp/elp_xxxxxx on unix.
        let tmp = Builder::new().prefix("elp_").tempdir().unwrap();
        let outdir = PathBuf::from(tmp.path());
        let (_stdout, stderr, code) = elp(args_vec![
            "parse-all",
            "--project",
            "../../test_projects/standard",
            "--to",
            tmp.path(),
        ]);
        // Now check .etf files were generated.
        let exists = |p| outdir.join(p).exists();
        assert!(exists("app_a.etf"));
        assert!(exists("app_a_SUITE.etf"));
        assert!(exists("app_a_mod2.etf"));
        assert!(exists("app_b.etf"));
        // The source file has a syntax error, so we don't create an etf
        assert!(!exists("parse_error_a_example_bad.etf"));
        assert_eq!(code, 0);
        assert!(stderr.is_empty())
    }

    fn parse_all_complete(project: &str) -> Result<()> {
        // Just check the command returns.
        let project_path = format!("../../test_projects/{}", project);
        let tmp = Builder::new().prefix("elp_parse_all_").tempdir().unwrap();
        let (_stdout, _stderr, _code) = elp(args_vec![
            "parse-all",
            "--project",
            project_path,
            "--to",
            tmp.path(),
        ]);
        Ok(())
    }

    fn eqwalize_snapshot(project: &str, module: &str, fast: bool) -> Result<()> {
        let project_path = format!("../../test_projects/{}", project);
        let fast_str = if fast { "_fast" } else { "" };
        let exp_path = format!(
            "../resources/test/{}/eqwalize_{}{}.pretty",
            project, module, fast_str
        );
        let expected = expect_file!(exp_path);

        let (stdout, stderr, code) = elp(args_vec!["eqwalize", "--project", project_path, module]);
        match code {
            0 => {
                expected.assert_eq(&stdout);
                assert!(stderr.is_empty())
            }
            _ => {
                expected.assert_eq(&stderr);
                assert!(stdout.is_empty());
            }
        }
        Ok(())
    }

    #[test]
    fn elp_parse_all_report_compile_error() {
        // We just check the process doesn't hang. See T114609762.
        parse_all_complete("parse_error").unwrap();
    }

    #[test]
    fn eqwalize_diagnostics_match_snapshot() {
        eqwalize_snapshot("standard", "app_a", false).unwrap();
        eqwalize_snapshot("standard", "app_a_mod2", true).unwrap();
        eqwalize_snapshot("standard", "app_a_lists", true).unwrap();
    }

    #[test]
    fn eqwalize_fails_on_bad_module_name() {
        eqwalize_snapshot("standard", "meinong", false).unwrap();
    }

    #[test]
    fn eqwalize_fails_on_bad_parse() {
        eqwalize_snapshot("parse_error", "parse_error_a_bad", false).unwrap();
    }

    #[test]
    fn eqwalize_fails_on_bad_parse_of_related_module() {
        eqwalize_snapshot("parse_error", "parse_error_a_reference_bad", false).unwrap();
    }

    #[test]
    fn eqwalize_succeeds_even_when_unrelated_module_has_parse_error() {
        eqwalize_snapshot("parse_error", "parse_error_a", false).unwrap();
    }

    #[test]
    fn eqwalize_module_diagnostics_match_snapshot_jsonl() {
        simple_snapshot(
            args_vec![
                "eqwalize",
                "app_a_mod2",
                "--project",
                "../../test_projects/standard",
                "--format",
                "json"
            ],
            expect_file!["../resources/test/standard/eqwalize_app_a_mod2_diagnostics.jsonl"],
        );
    }

    #[test]
    fn eqwalize_module_diagnostics_match_snapshot_json_lsp() {
        simple_snapshot(
            args_vec![
                "eqwalize",
                "app_a_mod2",
                "--project",
                "../../test_projects/standard",
                "--format",
                "json-lsp"
            ],
            expect_file!["../resources/test/standard/eqwalize_app_a_mod2_diagnostics_lsp.jsonl"],
        );
    }

    #[test]
    fn eqwalize_all_diagnostics_match_snapshot_jsonl() {
        simple_snapshot(
            args_vec![
                "eqwalize-all",
                "--project",
                "../../test_projects/standard",
                "--format",
                "json"
            ],
            expect_file!["../resources/test/standard/eqwalize_all_diagnostics.jsonl"],
        );
    }

    #[test]
    fn eqwalize_all_diagnostics_match_snapshot_pretty() {
        simple_snapshot(
            args_vec!["eqwalize-all", "--project", "../../test_projects/standard"],
            expect_file!["../resources/test/standard/eqwalize_all_diagnostics.pretty"],
        );
    }

    #[test]
    fn help() {
        let (stdout, stderr, code) = elp(args_vec!["--help"]);
        let expected = expect_file!["../resources/test/help.stderr"];
        assert_eq!(
            code, 0,
            "failed with exit code: {}\nstdout:\n{}\nstderr:\n{}",
            code, stdout, stderr
        );
        expected.assert_eq(&stderr);
        assert!(
            stdout.is_empty(),
            "expected stdout to be empty, got:\n{}",
            stdout,
        )
    }

    #[test]
    fn eqwalize_all_fails_on_bad_parse() {
        simple_snapshot(
            args_vec![
                "eqwalize-all",
                "--project",
                "../../test_projects/parse_error",
                "--format",
                "json",
            ],
            expect_file!["../resources/test/standard/eqwalize_all_parse_error.jsonl"],
        );
    }

    fn simple_snapshot(args: Vec<OsString>, expected: ExpectFile) {
        let (stdout, stderr, code) = elp(args);
        assert_eq!(
            code, 0,
            "failed with exit code: {}\nstdout:\n{}\nstderr:\n{}",
            code, stdout, stderr
        );
        expected.assert_eq(&stdout);
        assert!(
            stderr.is_empty(),
            "expected stderr to be empty, got:\n{}",
            stderr
        )
    }
}
