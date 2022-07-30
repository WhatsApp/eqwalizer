/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsString;
use std::path::PathBuf;

use anyhow::bail;
use anyhow::Result;
use fxhash::FxHashSet;
use pico_args::Arguments;

#[derive(Debug, Clone)]
pub struct Args {
    pub command: Command,
}

#[derive(Debug, Clone)]
pub enum Command {
    Version,
    Help,
    Eqwalize(Eqwalize),
    EqwalizeAll(EqwalizeAll),
    ParseAll(ParseAll),
}

#[derive(Debug, Clone)]
pub struct Eqwalize {
    pub project: PathBuf,
    pub module: String,
    pub fast: bool,
    pub profile: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EqwalizeAll {
    pub project: PathBuf,
    pub profile: Option<String>,
    pub json: bool,
}

#[derive(Debug, Clone)]
pub enum ParseAllInclude {
    AllModules,
    Modules(FxHashSet<String>),
}

#[derive(Debug, Clone)]
pub struct ParseAll {
    pub project: PathBuf,
    pub to: PathBuf,
    pub offset_positions: bool,
    pub profile: Option<String>,
    pub include: ParseAllInclude,
}

pub const HELP: &str = "\
elp

USAGE:
    elp [FLAGS] [COMMAND] [COMMAND_OPTIONS]

FLAGS:
    -h, --help                 Print this help
    --as profile               Rebar3 profile to pickup (default is test)

COMMANDS:
    version                    Print version
    help                       Print this help
    parse-all                  Dump ast for all files in a project for specified rebar.config file
        --project              Path to directory with rebar project
        --to                   Path to a directory where to dump .etf/.beam files
        --offset-positions     Emit positions as {StartByte, EndByte} rather than {Line, Col}
        --module               Only process indicated module(s) (can be repeated)
    eqwalize <module>          Eqwalize specified module
        --project              Path to directory with rebar project (defaults to `.`)
        --fast                 Refresh AST information for only the specified module
    eqwalize-all               Eqwalize all opted-in modules in a project (modules with `-typing([eqwalizer])`)
        --project              Path to directory with rebar project (defaults to `.`)
        --format json          Show diagnostics in JSON format

ENV VARS:
    ELP_EQWALIZER_PATH         Path to the eqwalizer executable, otherwise local one is used

";

impl Args {
    pub fn parse(args: Vec<OsString>) -> Result<Args> {
        let mut arguments = Arguments::from_vec(args);

        if arguments.contains(["-h", "--help"]) {
            return Ok(Args {
                command: Command::Help,
            });
        }

        let profile = arguments.opt_value_from_str("--as")?;

        let command = match arguments.subcommand()? {
            Some(command) => command,
            None => bail!("Command is expected"),
        };

        let command = match command.as_str() {
            "version" => Command::Version,
            "help" => Command::Help,
            "parse-all" => {
                let project = get_project(&mut arguments)?;
                let to = arguments.value_from_str("--to")?;
                let offset_positions = arguments.contains("--offset-positions");
                let include = {
                    let modules = arguments
                        .values_from_str("--module")?
                        .into_iter()
                        .collect::<FxHashSet<_>>();
                    if modules.is_empty() {
                        ParseAllInclude::AllModules
                    } else {
                        ParseAllInclude::Modules(modules)
                    }
                };
                Command::ParseAll(ParseAll {
                    project,
                    to,
                    profile,
                    offset_positions,
                    include,
                })
            }
            "eqwalize" => {
                let project = get_project(&mut arguments)?;
                let module = arguments.free_from_str()?;
                let fast = arguments.contains("--fast");
                Command::Eqwalize(Eqwalize {
                    project,
                    module,
                    fast,
                    profile,
                })
            }
            "eqwalize-all" => {
                let project = get_project(&mut arguments)?;
                let format: String = arguments
                    .opt_value_from_str("--format")?
                    .unwrap_or_default();
                let json = format == "json";
                Command::EqwalizeAll(EqwalizeAll {
                    project,
                    profile,
                    json,
                })
            }
            _ => Command::Help,
        };

        finish_args(arguments)?;
        Ok(Args { command })
    }
}

fn get_project(args: &mut Arguments) -> Result<PathBuf> {
    let project = args.opt_value_from_str("--project")?;
    Ok(project.unwrap_or(PathBuf::from(".")))
}

fn finish_args(args: Arguments) -> Result<()> {
    let unexpected = args.finish();
    if !unexpected.is_empty() {
        bail!("Unexpected arguments: {:?}", unexpected);
    }
    Ok(())
}
