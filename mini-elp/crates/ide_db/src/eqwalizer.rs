/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use anyhow::Result;
use elp_base_db::AbsPath;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_eqwalizer::EqwalizerDiagnostics;
use salsa::Database;

use crate::ErlAstDatabase;

pub trait EqwalizerLoader {
    fn typecheck(
        &self,
        project_id: ProjectId,
        build_info_path: &AbsPath,
        modules: Vec<&str>,
        format: elp_parse_server::Format,
    ) -> Result<EqwalizerDiagnostics>;
}

impl EqwalizerLoader for crate::RootDatabase {
    fn typecheck(
        &self,
        project_id: ProjectId,
        build_info_path: &AbsPath,
        modules: Vec<&str>,
        format: elp_parse_server::Format,
    ) -> Result<EqwalizerDiagnostics> {
        let db_api = DbForEqwalizer {
            db: self,
            project_id,
            format,
        };

        self.eqwalizer
            .typecheck(build_info_path.as_ref(), db_api, modules)
    }
}

struct DbForEqwalizer<'d> {
    db: &'d crate::RootDatabase,
    project_id: ProjectId,
    format: elp_parse_server::Format,
}

#[salsa::query_group(EqwalizerDatabaseStorage)]
pub trait EqwalizerDatabase: SourceDatabase + EqwalizerLoader + ErlAstDatabase {
    fn eqwalizer_diagnostics(
        &self,
        project_id: ProjectId,
        file_ids: Vec<FileId>,
        format: elp_parse_server::Format,
    ) -> Arc<EqwalizerDiagnostics>;
}

fn eqwalizer_diagnostics(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    file_ids: Vec<FileId>,
    format: elp_parse_server::Format,
) -> Arc<EqwalizerDiagnostics> {
    let module_index = db.module_index(project_id);
    let module_names: Vec<&str> = file_ids
        .into_iter()
        .map(|f| -> &str { module_index.module_for_file(f).unwrap() })
        .collect();
    let project = db.project_data(project_id);
    if let Some(build_info_path) = &project.build_info_path {
        match db.typecheck(project_id, build_info_path, module_names, format) {
            Ok(diags) => Arc::new(diags),
            Err(error) => {
                log::error!("EqWAlizing failed: {}", error);
                Default::default()
            }
        }
    } else {
        log::error!("EqWAlizing in a fixture project");
        Default::default()
    }
}

impl<'d> elp_eqwalizer::DbApi for DbForEqwalizer<'d> {
    fn unwind_if_cancelled(&self) -> () {
        self.db.unwind_if_cancelled()
    }

    fn get_ast(&self, module: &str) -> Option<Arc<Vec<u8>>> {
        let file_id = self
            .db
            .module_index(self.project_id)
            .file_for_module(module)?;
        self.db.module_ast(file_id, self.format).ok()
    }
}
