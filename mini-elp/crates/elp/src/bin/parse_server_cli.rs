/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::Path;
use std::str;

use anyhow::Context;
use anyhow::Result;
use elp::convert;
use elp_ide::parse_server;
use elp_ide::Analysis;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::LineCol;
use elp_parse_server::Location;
use elp_project_model::AppType;
use elp_project_model::Profile;
use indicatif::ParallelProgressIterator;
use rayon::prelude::*;
use text_size::TextRange;

use crate::args::ParseAll;
use crate::args::ParseAllInclude;
use crate::load_rebar;
use crate::load_rebar::LoadResult;
use crate::reporting;
use crate::reporting::ParseDiagnostic;
use crate::util;

pub fn parse_all(args: &ParseAll, out: &mut impl std::io::Write) -> Result<()> {
    let profile = args.profile.clone().map(Profile).unwrap_or_default();
    let loaded = load_rebar::load_project_at(&args.project, &profile)?;
    fs::create_dir_all(&args.to)?;
    let format = match args.offset_positions {
        true => parse_server::Format::OffsetEtf,
        false => parse_server::Format::Etf,
    };

    util::compile_deps(&loaded)?;
    let parse_diagnostics = do_parse_all(&loaded, &args.to, format, &args.include)?;
    if !parse_diagnostics.is_empty() {
        writeln!(
            out,
            "{}",
            reporting::format_raw_parse_error(&parse_diagnostics)
        )
        .unwrap();
    }
    Ok(())
}

pub fn do_parse_all(
    loaded: &LoadResult,
    to: &Path,
    format: parse_server::Format,
    include: &ParseAllInclude,
) -> Result<Vec<ParseDiagnostic>> {
    let file_cnt = loaded.vfs.len();

    let pb = util::progress(file_cnt as u64, "Parsing modules", "Parsed modules");
    let mut result = loaded
        .analysis()
        .module_index(loaded.project_id)?
        .iter()
        .par_bridge()
        .progress_with(pb)
        .map_with(
            loaded.analysis(),
            move |db, (name, _, file_id)| -> Result<Vec<ParseDiagnostic>> {
                let empty = Ok(vec![]);
                match include {
                    ParseAllInclude::AllModules => {}
                    ParseAllInclude::Modules(modules) if !modules.contains(name.as_str()) => {
                        return empty;
                    }
                    _ => {}
                }
                if db.file_app_type(file_id).ok() == Some(Some(AppType::Dep)) {
                    return empty;
                }

                do_parse_one(db, Some((name, to)), file_id, format)
                    .with_context(|| format!("Failed to parse module {}", name.as_str()))
            },
        )
        .try_reduce(std::vec::Vec::new, |mut acc, diagnostics| {
            acc.extend(diagnostics);
            Ok(acc)
        })?;
    result.sort_by(|f, l| f.relative_path.cmp(&l.relative_path));
    Ok(result)
}

pub fn do_parse_one(
    db: &Analysis,
    to: Option<(&str, &Path)>,
    file_id: FileId,
    format: parse_server::Format,
) -> Result<Vec<ParseDiagnostic>> {
    let ext = match format {
        parse_server::Format::Etf | parse_server::Format::OffsetEtf { .. } => "etf",
        parse_server::Format::Text => panic!("text format is for test purposes only!"),
    };

    match db.module_ast(file_id, format)? {
        Ok(module_ast) => {
            if let Some((name, to)) = to {
                let to_path = to.join(format!("{}.{}", name, ext));
                fs::write(to_path, &*module_ast)?;
            }
            Ok(vec![])
        }
        Err(errs) => {
            let line_index = db.line_index(file_id)?;
            let root_dir = &db.project_data(file_id)?.unwrap().root_dir;
            let diagnostic = errs
                .iter()
                .map(|err| {
                    let relative_path: &Path = err.path.strip_prefix(root_dir).unwrap_or(&err.path);
                    let (range, line_num) = match err.location {
                        None => (None, convert::position(&line_index, 0.into()).line + 1),
                        Some(Location::TextRange(range)) => (
                            Some(range),
                            convert::position(&line_index, range.start()).line + 1,
                        ),
                        Some(Location::StartLocation(start)) => {
                            let offset = line_index.offset(LineCol {
                                line: start.line - 1,
                                col_utf16: start.column - 1,
                            });
                            (Some(TextRange::empty(offset)), start.line)
                        }
                    };
                    ParseDiagnostic {
                        file_id,
                        relative_path: relative_path.to_owned(),
                        line_num,
                        msg: err.msg.to_owned(),
                        range,
                    }
                })
                .collect();
            Ok(diagnostic)
        }
    }
}

// ---------------------------------------------------------------------
