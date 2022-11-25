/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::io::Write;
use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::str;

use anyhow::Context;
use anyhow::Result;
use codespan_reporting::diagnostic::Diagnostic as ReportingDiagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::Color;
use codespan_reporting::term::termcolor::ColorSpec;
use codespan_reporting::term::termcolor::WriteColor;
use elp::arc_types;
use elp::convert;
use elp_ide::Analysis;
use elp_ide_db::elp_base_db::AbsPath;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::VfsPath;
use elp_ide_db::EqwalizerDiagnostic;
use lazy_static::lazy_static;
use lsp_types::Diagnostic;
use serde::Serialize;
use text_size::TextRange;

use crate::load_rebar::LoadResult;

pub trait Reporter {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()>;
    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()>;
    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()>;
    fn write_error_count(&mut self) -> Result<()>;
}

#[derive(Debug, Clone)]
pub struct ParseDiagnostic {
    pub file_id: FileId,
    pub relative_path: PathBuf,
    pub line_num: u32,
    pub msg: String,
    pub range: Option<TextRange>,
}

#[derive(Serialize)]
pub struct LSPDiagnosticWithPath {
    pub relative_path: PathBuf,
    pub diagnostic: Diagnostic,
}

pub struct PrettyReporter<'a, W: WriteColor> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    out: &'a mut W,
    error_count: usize,
}

pub struct JsonReporter<'a, W: Write> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    out: &'a mut W,
}

pub struct JsonLSPReporter<'a, W: Write> {
    analysis: &'a Analysis,
    loaded: &'a LoadResult,
    out: &'a mut W,
}

impl<'a, W> PrettyReporter<'a, W>
where
    W: WriteColor,
{
    pub fn new(analysis: &'a Analysis, loaded: &'a LoadResult, out: &'a mut W) -> Self {
        Self {
            analysis,
            loaded,
            out,
            error_count: 0,
        }
    }

    fn get_reporting_data(&self, file_id: FileId) -> Result<(SimpleFiles<String, &'a str>, usize)> {
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        let content = str::from_utf8(self.loaded.vfs.file_contents(file_id)).unwrap();
        let mut files: SimpleFiles<String, &str> = SimpleFiles::new();
        let id = files.add(relative_path.display().to_string(), content);
        Ok((files, id))
    }
}

impl<'a, W: WriteColor> Reporter for PrettyReporter<'a, W> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let (reporting_files, reporting_id) = self.get_reporting_data(file_id)?;
        for diagnostic in diagnostics {
            let range: Range<usize> =
                diagnostic.range.start().into()..diagnostic.range.end().into();
            let expr = match &diagnostic.expression {
                Some(s) => format!("{}.\n", s),
                None => "".to_string(),
            };

            let msg = format!("{}{}\n", expr, diagnostic.message);
            let msg_label = Label::primary(reporting_id, range.clone()).with_message(&msg);
            let mut labels = vec![msg_label];
            if let Some(s) = &diagnostic.explanation {
                let explanation_label =
                    Label::secondary(reporting_id, range).with_message(format!("\n\n{}", s));
                labels.push(explanation_label);
            };
            let d: ReportingDiagnostic<usize> = ReportingDiagnostic::error()
                .with_message(&diagnostic.code)
                .with_labels(labels);

            term::emit(self.out, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        }
        self.error_count = self.error_count + diagnostics.len();
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let range = diagnostic.range.unwrap_or_default();
            let range: Range<usize> = range.start().into()..range.end().into();
            let (reporting_files, reporting_id) = self.get_reporting_data(diagnostic.file_id)?;
            let label = Label::primary(reporting_id, range).with_message(&diagnostic.msg);
            let d: ReportingDiagnostic<usize> = ReportingDiagnostic::error()
                .with_message("parse_error")
                .with_labels(vec![label]);
            term::emit(self.out, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let (reporting_files, reporting_id) = self.get_reporting_data(file_id)?;
        let label = Label::primary(reporting_id, 1..2).with_message(&description);
        let d: ReportingDiagnostic<usize> = ReportingDiagnostic::note()
            .with_message("advice")
            .with_labels(vec![label]);
        term::emit(self.out, &REPORTING_CONFIG, &reporting_files, &d).unwrap();
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        if self.error_count == 0 {
            self.out.set_color(&GREEN_COLOR_SPEC)?;
            writeln!(self.out, "NO ERRORS")?;
            self.out.reset()?;
        } else {
            self.out.set_color(&CYAN_COLOR_SPEC)?;
            let noun = if self.error_count == 1 {
                "ERROR"
            } else {
                "ERRORS"
            };
            writeln!(self.out, "{} {}", self.error_count, noun)?;
            self.out.reset()?;
        }
        Ok(())
    }
}

impl<'a, W: Write> JsonReporter<'a, W> {
    pub fn new(analysis: &'a Analysis, loaded: &'a LoadResult, out: &'a mut W) -> Self {
        Self {
            analysis,
            loaded,
            out,
        }
    }
}

impl<'a, W: Write> Reporter for JsonReporter<'a, W> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let line_index = self.analysis.line_index(file_id);
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        for diagnostic in diagnostics {
            let diagnostic =
                convert::eqwalizer_to_arc_diagnostic(diagnostic, &line_index, relative_path);
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.out, "{}", diagnostic)?;
        }
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let diagnostic = arc_types::Diagnostic::new(
                diagnostic.relative_path.as_path(),
                diagnostic.line_num,
                None,
                "ELP".to_string(),
                diagnostic.msg.clone(),
                None,
            );
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.out, "{}", diagnostic)?;
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        let diagnostic = arc_types::Diagnostic::new(
            relative_path,
            1,
            None,
            "ELP".to_string(),
            description,
            None,
        );
        let diagnostic = serde_json::to_string(&diagnostic)?;
        writeln!(self.out, "{}", diagnostic)?;
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        Ok(())
    }
}

impl<'a, W: Write> JsonLSPReporter<'a, W> {
    pub fn new(analysis: &'a Analysis, loaded: &'a LoadResult, out: &'a mut W) -> Self {
        Self {
            analysis,
            loaded,
            out,
        }
    }
}

impl<'a, W: Write> Reporter for JsonLSPReporter<'a, W> {
    fn write_eqwalizer_diagnostics(
        &mut self,
        file_id: FileId,
        diagnostics: &[EqwalizerDiagnostic],
    ) -> Result<()> {
        let line_index = self.analysis.line_index(file_id);
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        for diagnostic in diagnostics {
            let diagnostic = convert::eqwalizer_to_lsp_diagnostic(diagnostic, &line_index);
            let diagnostic_with_path = LSPDiagnosticWithPath {
                diagnostic,
                relative_path: relative_path.to_path_buf(),
            };
            let diagnostic_with_path = serde_json::to_string(&diagnostic_with_path)?;
            writeln!(self.out, "{}", diagnostic_with_path)?;
        }
        Ok(())
    }

    fn write_parse_diagnostics(&mut self, diagnostics: &[ParseDiagnostic]) -> Result<()> {
        for diagnostic in diagnostics {
            let diagnostic = arc_types::Diagnostic::new(
                diagnostic.relative_path.as_path(),
                diagnostic.line_num,
                None,
                "ELP".to_string(),
                diagnostic.msg.clone(),
                None,
            );
            let diagnostic = serde_json::to_string(&diagnostic)?;
            writeln!(self.out, "{}", diagnostic)?;
        }
        Ok(())
    }

    fn write_file_advice(&mut self, file_id: FileId, description: String) -> Result<()> {
        let file_path = &self.loaded.vfs.file_path(file_id);
        let root_path = &self
            .analysis
            .project_data(file_id)
            .with_context(|| "could not find project data")?
            .root_dir;
        let relative_path = get_relative_path(root_path, file_path);
        let diagnostic = arc_types::Diagnostic::new(
            relative_path,
            1,
            None,
            "ELP".to_string(),
            description,
            None,
        );
        let diagnostic = serde_json::to_string(&diagnostic)?;
        writeln!(self.out, "{}", diagnostic)?;
        Ok(())
    }

    fn write_error_count(&mut self) -> Result<()> {
        Ok(())
    }
}

pub fn format_raw_parse_error(errs: &[ParseDiagnostic]) -> String {
    errs.iter()
        .map(|err| {
            format!(
                "{}:{} {}",
                err.relative_path.display(),
                err.line_num,
                err.msg,
            )
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn get_relative_path<'a>(root: &AbsPath, file: &'a VfsPath) -> &'a Path {
    let file = file.as_path().unwrap();
    match file.strip_prefix(root) {
        Some(relative) => relative.as_ref(),
        None => file.as_ref(),
    }
}

lazy_static! {
    static ref REPORTING_CONFIG: term::Config = {
        let mut config = codespan_reporting::term::Config::default();
        config
            .styles
            .primary_label_error
            .set_fg(Some(Color::Ansi256(9)));
        config.styles.line_number.set_fg(Some(Color::Ansi256(33)));
        config.styles.source_border.set_fg(Some(Color::Ansi256(33)));
        config
    };
    static ref GREEN_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Green));
        spec
    };
    static ref CYAN_COLOR_SPEC: ColorSpec = {
        let mut spec = ColorSpec::default();
        spec.set_fg(Some(Color::Cyan));
        spec
    };
}
