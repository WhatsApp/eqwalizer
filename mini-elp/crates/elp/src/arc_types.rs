/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

/// Types as defined in https://www.internalfb.com/intern/wiki/Linting/adding-linters/#flow-type
/// and https://www.internalfb.com/code/whatsapp-server/[4dcee4c563dd9d160ad885069a816907216c9e40]/erl/tools/lint/arcanist.py?lines=17 /
use std::path::Path;

use serde::Serialize;

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Diagnostic {
    // Filepath
    path: String,
    line: Option<u32>,
    char: Option<u32>,
    // Linter name (normally this would need to match code in fbsource-lint-engine.toml)
    code: String,
    // Rule name
    name: String,
    original: Option<String>,
    replacement: Option<String>,
    description: Option<String>,
}

impl Diagnostic {
    pub fn new(
        path: &Path,
        line: u32,
        character: Option<u32>,
        name: String,
        description: String,
        original: Option<String>,
    ) -> Self {
        Diagnostic {
            path: path.display().to_string(), // lossy on Windows for unicode paths
            line: Some(line),
            r#char: character,
            code: "ELP".to_owned(),
            name,
            original,
            replacement: None,
            description: Some(description),
        }
    }
}
