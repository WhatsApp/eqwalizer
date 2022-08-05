/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use elp_eqwalizer::EqwalizerDiagnostic;
use elp_ide_db::LineIndex;
use text_size::TextRange;
use text_size::TextSize;

use crate::arc_types;

pub fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);
    lsp_types::Position::new(line_col.line, line_col.col_utf16)
}

pub fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    let start = position(line_index, range.start());
    let end = position(line_index, range.end());
    lsp_types::Range::new(start, end)
}

pub fn eqwalizer_to_arc_diagnostic(
    d: &EqwalizerDiagnostic,
    line_index: &LineIndex,
    relative_path: &Path,
) -> arc_types::Diagnostic {
    let pos = position(line_index, d.range.start());
    let line_num = pos.line + 1;
    let character = Some(pos.character + 1);
    // formatting: https://fburl.com/max_wiki_link_to_phabricator_rich_text
    let explanation = match &d.explanation {
        Some(s) => format!("```\n{}\n```", s),
        None => "".to_string(),
    };
    let link = format!("> [docs on `{}`]({})", d.code, d.uri);
    let message = format!(
        "```lang=error,counterexample
{}
{}
```
{}
{}",
        expr_string(d),
        d.message,
        explanation,
        link
    );
    let name = format!("eqWAlizer: {}", d.code);
    arc_types::Diagnostic::new(
        relative_path,
        line_num,
        character,
        name,
        message,
        d.expression.clone(),
    )
}

pub fn eqwalizer_to_lsp_diagnostic(
    d: &EqwalizerDiagnostic,
    line_index: &LineIndex,
) -> lsp_types::Diagnostic {
    let range = range(line_index, d.range);
    let severity = lsp_types::DiagnosticSeverity::WARNING;
    let explanation = match &d.explanation {
        Some(s) => format!("\n\n{}", s),
        None => "".to_string(),
    };
    let message = format!("{}{}\n", d.message, explanation);
    lsp_types::Diagnostic {
        range,
        severity: Some(severity),
        code: Some(lsp_types::NumberOrString::String("eqwalizer".to_string())),
        code_description: None,
        source: Some("elp".into()),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn expr_string(d: &EqwalizerDiagnostic) -> String {
    match &d.expression {
        Some(s) => format!("`{}`.\n", s),
        None => "".to_string(),
    }
}
