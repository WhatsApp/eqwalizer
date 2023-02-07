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
        modules: Vec<FileId>,
        format: elp_parse_server::Format,
        strict: bool,
    ) -> Result<EqwalizerDiagnostics>;
}

impl EqwalizerLoader for crate::RootDatabase {
    fn typecheck(
        &self,
        project_id: ProjectId,
        build_info_path: &AbsPath,
        modules: Vec<FileId>,
        format: elp_parse_server::Format,
        strict: bool,
    ) -> Result<EqwalizerDiagnostics> {
        let module_index = self.module_index(project_id);
        let module_names = modules
            .iter()
            .map(|&f| -> &str { module_index.module_for_file(f).unwrap() })
            .collect();
        let db_api = DbForEqwalizer {
            db: self,
            total: modules.len(),
            left: modules.len(),
            project_id,
            format,
        };

        self.eqwalizer
            .typecheck(build_info_path.as_ref(), db_api, module_names, strict)
    }
}

struct DbForEqwalizer<'d> {
    db: &'d crate::RootDatabase,
    total: usize,
    left: usize,
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
        strict: bool,
    ) -> Arc<EqwalizerDiagnostics>;
}

fn eqwalizer_diagnostics(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    file_ids: Vec<FileId>,
    format: elp_parse_server::Format,
    strict: bool,
) -> Arc<EqwalizerDiagnostics> {
    let project = db.project_data(project_id);
    if let Some(build_info_path) = &project.build_info_path {
        match db.typecheck(project_id, build_info_path, file_ids, format, strict) {
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

    fn get_ast(&mut self, module: &str) -> Option<Arc<Vec<u8>>> {
        let file_id = self
            .db
            .module_index(self.project_id)
            .file_for_module(module)?;
        self.db.module_ast(file_id, self.format).ok()
    }

    fn eqwalizing_start(&self, module: String) -> () {
        if let Some(reporter) = self.db.eqwalizer_progress_reporter.lock().unwrap().as_ref() {
            reporter.report_module(module)
        }
    }

    fn eqwalizing_done(&mut self, _module: String) -> () {
        self.left -= 1;
        if let Some(reporter) = self.db.eqwalizer_progress_reporter.lock().unwrap().as_ref() {
            reporter.report(self.total - self.left);
            if self.left == 0 {
                reporter.clean_module();
                reporter.finish();
            }
        }
    }
}
