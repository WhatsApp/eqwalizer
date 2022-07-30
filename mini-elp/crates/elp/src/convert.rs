/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use elp_eqwalizer::EqwalizerDiagnostic;
use elp_ide_db::LineIndex;
use text_size::TextSize;

use crate::arc_types;

pub fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.line_col(offset);
    lsp_types::Position::new(line_col.line, line_col.col_utf16)
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

fn expr_string(d: &EqwalizerDiagnostic) -> String {
    match &d.expression {
        Some(s) => format!("`{}`.\n", s),
        None => "".to_string(),
    }
}
