/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use codespan_reporting::term::termcolor::WriteColor;
use elp_ide::Analysis;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileSource;
use elp_ide_db::elp_base_db::ModuleIndex;
use elp_ide_db::EqwalizerDiagnostics;
use elp_project_model::AppType;
use elp_project_model::Profile;
use rayon::prelude::*;

use crate::args::Eqwalize;
use crate::args::EqwalizeAll;
use crate::args::EqwalizeApp;
use crate::args::Format;
use crate::load_rebar;
use crate::load_rebar::LoadResult;
use crate::parse_server_cli;
use crate::reporting;
use crate::util;

struct EqwalizerInternalArgs<'a> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    file_ids: Vec<FileId>,
    reporter: &'a mut dyn reporting::Reporter,
    fast: bool,
    strict: bool,
}

pub fn eqwalize_module(args: &Eqwalize, mut out: impl WriteColor) -> Result<i32> {
    let loaded = &match args.project.extension() {
        None => {
            let profile = args.profile.clone().map(Profile).unwrap_or_default();
            load_rebar::load_project_with_caching_at(&args.project, &profile, args.fast)
        }
        Some(ext) => match ext.to_str() {
            Some("json") => load_rebar::load_json_project_at(&args.project),
            _ => panic!("Unknown file type for option --project"),
        },
    }?;
    let analysis = &loaded.analysis();
    let file_id = analysis
        .module_file_id(loaded.project_id, &args.module)
        .with_context(|| format!("Module {} not found", &args.module))?;
    let mut reporter: Box<dyn reporting::Reporter> = match args.format {
        Format::Json => Box::new(reporting::JsonReporter::new(analysis, loaded, &mut out)),
        Format::JsonLSP => Box::new(reporting::JsonLSPReporter::new(analysis, loaded, &mut out)),
        Format::Pretty => Box::new(reporting::PrettyReporter::new(analysis, loaded, &mut out)),
    };
    let reporter = reporter.as_mut();
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids: vec![file_id],
        reporter,
        fast: args.fast,
        strict: args.strict,
    })
}

pub fn eqwalize_all(args: &EqwalizeAll, mut out: impl WriteColor) -> Result<i32> {
    let loaded = &match args.project.extension() {
        None => {
            let profile = args.profile.clone().map(Profile).unwrap_or_default();
            load_rebar::load_project_at(&args.project, &profile)
        }
        Some(ext) => match ext.to_str() {
            Some("json") => load_rebar::load_json_project_at(&args.project),
            _ => panic!("Unknown file type for option --project"),
        },
    }?;
    let analysis = &loaded.analysis();

    let mut reporter: Box<dyn reporting::Reporter> = match args.format {
        Format::Json => Box::new(reporting::JsonReporter::new(analysis, loaded, &mut out)),
        Format::JsonLSP => Box::new(reporting::JsonLSPReporter::new(analysis, loaded, &mut out)),
        Format::Pretty => Box::new(reporting::PrettyReporter::new(analysis, loaded, &mut out)),
    };
    let reporter = reporter.as_mut();
    let module_index = analysis.module_index(loaded.project_id);
    let file_ids: Vec<FileId> = module_index
        .iter()
        .map(|(_name, _source, file_id)| file_id)
        .filter_map(|file_id| {
            if should_eqwalize(analysis, &module_index, file_id) {
                Some(file_id)
            } else {
                None
            }
        })
        .collect();
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
        fast: false,
        strict: args.strict,
    })
}

pub fn eqwalize_app(args: &EqwalizeApp, mut out: impl WriteColor) -> Result<i32> {
    let loaded = &match args.project.extension() {
        None => {
            let profile = args.profile.clone().map(Profile).unwrap_or_default();
            load_rebar::load_project_at(&args.project, &profile)
        }
        Some(ext) => match ext.to_str() {
            Some("json") => load_rebar::load_json_project_at(&args.project),
            _ => panic!("Unknown file type for option --project"),
        },
    }?;
    let analysis = &loaded.analysis();

    let mut reporter: Box<dyn reporting::Reporter> = match args.format {
        Format::Json => Box::new(reporting::JsonReporter::new(analysis, loaded, &mut out)),
        Format::JsonLSP => Box::new(reporting::JsonLSPReporter::new(analysis, loaded, &mut out)),
        Format::Pretty => Box::new(reporting::PrettyReporter::new(analysis, loaded, &mut out)),
    };
    let reporter = reporter.as_mut();
    let module_index = analysis.module_index(loaded.project_id);
    let file_ids: Vec<FileId> = module_index
        .iter()
        .filter_map(|(_name, _source, file_id)| {
            if analysis.file_app_name(file_id) == Some(args.app.clone())
                && should_eqwalize(analysis, &module_index, file_id)
            {
                Some(file_id)
            } else {
                None
            }
        })
        .collect();
    eqwalize(EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
        fast: false,
        strict: args.strict,
    })
}

fn eqwalize(
    EqwalizerInternalArgs {
        analysis,
        loaded,
        file_ids,
        reporter,
        fast,
        strict,
    }: EqwalizerInternalArgs,
) -> Result<i32> {
    let format = elp_parse_server::Format::OffsetEtf;

    if !fast {
        util::compile_deps(&loaded)?;
    }
    pre_parse_for_speed(analysis.clone(), &file_ids, format);
    let pb = util::progress(file_ids.len() as u64, "eqWAlizing", "eqWAlized");
    let output = loaded.with_eqwalizer_progress_bar(pb.clone(), move |analysis| {
        analysis.eqwalizer_diagnostics(loaded.project_id, file_ids, format, strict)
    });
    pb.finish();
    match &*output {
        EqwalizerDiagnostics::Diagnostics(diagnostics_by_module) => {
            for (module, diagnostics) in diagnostics_by_module {
                let file_id = analysis
                    .module_index(loaded.project_id)
                    .file_for_module(module.as_str())
                    .with_context(|| format!("module {} not found", module))?;
                reporter.write_eqwalizer_diagnostics(file_id, diagnostics)?;
            }
            reporter.write_error_count()?;
            if diagnostics_by_module.is_empty() {
                Ok(0)
            } else {
                Ok(1)
            }
        }
        EqwalizerDiagnostics::NoAst { module } => {
            if let Some(file_id) = analysis.module_file_id(loaded.project_id, module) {
                let parse_diagnostics =
                    parse_server_cli::do_parse_one(analysis, None, file_id, format)?;
                // The cached parse errors must be non-empty otherwise we wouldn't have `NoAst`
                assert!(!parse_diagnostics.is_empty());
                reporter.write_parse_diagnostics(&parse_diagnostics)?;
                Ok(1)
            } else {
                bail!(
                    "Could not type-check because module {} was not found",
                    module
                )
            }
        }
    }
}

fn pre_parse_for_speed(analysis: Analysis, file_ids: &[FileId], format: elp_parse_server::Format) {
    file_ids
        .par_iter()
        .for_each_with(analysis, |analysis, &file_id| {
            let _ = analysis.module_ast(file_id, format);
        });
}

fn should_eqwalize(analysis: &Analysis, module_index: &ModuleIndex, file_id: FileId) -> bool {
    let is_in_app = analysis.file_app_type(file_id) == Some(AppType::App);
    is_in_app
        && module_index.module_for_file(file_id).is_some()
        && module_index.file_source_for_file(file_id) == Some(FileSource::Src)
}
