/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use anyhow::Result;
pub use elp_ide_db;
use elp_ide_db::elp_base_db::salsa;
use elp_ide_db::elp_base_db::salsa::ParallelDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::ModuleIndex;
use elp_ide_db::elp_base_db::ModuleName;
use elp_ide_db::elp_base_db::ProjectData;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::elp_base_db::SourceDatabase;
pub use elp_ide_db::parse_server;
use elp_ide_db::parse_server::ParseError;
use elp_ide_db::Eqwalizer;
use elp_ide_db::EqwalizerDatabase;
use elp_ide_db::EqwalizerDiagnostics;
use elp_ide_db::ErlAstDatabase;
use elp_ide_db::LineIndex;
use elp_ide_db::LineIndexDatabase;
use elp_ide_db::RootDatabase;
use elp_project_model::AppType;

/// `AnalysisHost` stores the current state of the world.
#[derive(Debug, Default)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    /// Returns a snapshot of the current state, which you can query for
    /// semantic information.
    pub fn analysis(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    pub fn raw_database(&self) -> &RootDatabase {
        &self.db
    }
    pub fn raw_database_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }
}

/// Analysis is a snapshot of a world state at a moment in time. It is the main
/// entry point for asking semantic information about the world. When the world
/// state is advanced using `AnalysisHost::apply_change` method, all existing
/// `Analysis` are canceled (most method return `Err(Canceled)`).
#[derive(Debug)]
pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

// As a general design guideline, `Analysis` API are intended to be independent
// from the language server protocol. That is, when exposing some functionality
// we should think in terms of "what API makes most sense" and not in terms of
// "what types LSP uses". We have at least 2 consumers of the API - LSP and CLI
impl Analysis {
    /// Gets the file's `LineIndex`: data structure to convert between absolute
    /// offsets and line/column representation.
    pub fn line_index(&self, file_id: FileId) -> Arc<LineIndex> {
        self.db.file_line_index(file_id)
    }

    /// Computes the set of eqwalizer diagnostics for the given file.
    pub fn eqwalizer_diagnostics(
        &self,
        project_id: ProjectId,
        file_ids: Vec<FileId>,
        format: parse_server::Format,
        strict: bool,
    ) -> Arc<EqwalizerDiagnostics> {
        self.db
            .eqwalizer_diagnostics(project_id, file_ids, format, strict)
    }

    /// Low-level access to eqwalizer
    pub fn eqwalizer(&self) -> &Eqwalizer {
        self.db.eqwalizer()
    }

    /// ETF for the module's abstract forms
    pub fn module_ast(
        &self,
        file_id: FileId,
        format: parse_server::Format,
    ) -> Result<Arc<Vec<u8>>, Arc<Vec<ParseError>>> {
        self.db.module_ast(file_id, format)
    }

    pub fn project_data(&self, file_id: FileId) -> Option<Arc<ProjectData>> {
        Some(
            self.db.project_data(
                self.db
                    .app_data(self.db.file_source_root(file_id))?
                    .project_id,
            ),
        )
    }

    /// Returns module name
    pub fn module_name(&self, file_id: FileId) -> Option<ModuleName> {
        let db = &self.db;
        let app_data = db.app_data(db.file_source_root(file_id))?;
        db.module_index(app_data.project_id)
            .module_for_file(file_id)
            .cloned()
    }

    pub fn module_index(&self, project_id: ProjectId) -> Arc<ModuleIndex> {
        self.db.module_index(project_id)
    }

    pub fn module_file_id(&self, project_id: ProjectId, module: &str) -> Option<FileId> {
        self.db.module_index(project_id).file_for_module(module)
    }

    /// Returns the app_type for a file
    pub fn file_app_type(&self, file_id: FileId) -> Option<AppType> {
        self.db.file_app_type(file_id)
    }
}

impl Clone for Analysis {
    fn clone(&self) -> Self {
        Analysis {
            db: self.db.snapshot(),
        }
    }
}
