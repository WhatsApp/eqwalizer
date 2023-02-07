/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::panic::RefUnwindSafe;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;

use anyhow::Result;
use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::FileLoader;
use elp_base_db::FileLoaderDelegate;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_base_db::Upcast;
use fxhash::FxHashMap;
use parse_server::Connection;
use salsa::Database;

pub mod eqwalizer;
mod erl_ast;
mod fixmes;
mod line_index;

// ---------------------------------------------------------------------
// pub mod fixture;

pub use elp_base_db;
pub use elp_eqwalizer::Eqwalizer;
pub use elp_eqwalizer::EqwalizerDiagnostic;
pub use elp_eqwalizer::EqwalizerDiagnostics;
pub use elp_parse_server as parse_server;
pub use eqwalizer::EqwalizerDatabase;
pub use erl_ast::ErlAstDatabase;
pub use line_index::LineCol;
pub use line_index::LineIndex;
// ---------------------------------------------------------------------

type EqwalizerProgressReporterBox = Arc<Mutex<Option<Box<dyn EqwalizerProgressReporter>>>>;

pub trait EqwalizerProgressReporter: Send + Sync + RefUnwindSafe {
    fn report(&self, done: usize);
    fn report_module(&self, module: String);
    fn clean_module(&self);
    fn finish(&self);
}

#[salsa::database(
    LineIndexDatabaseStorage,
    elp_base_db::SourceDatabaseExtStorage,
    elp_base_db::SourceDatabaseStorage,
    eqwalizer::EqwalizerDatabaseStorage,
    erl_ast::ErlAstDatabaseStorage
)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    parse_servers: Arc<RwLock<FxHashMap<ProjectId, Connection>>>,
    eqwalizer: Eqwalizer,
    eqwalizer_progress_reporter: EqwalizerProgressReporterBox,
}

impl Upcast<dyn SourceDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn SourceDatabase + 'static) {
        &*self
    }
}

impl FileLoader for RootDatabase {
    fn file_text(&self, file_id: FileId) -> Arc<String> {
        FileLoaderDelegate(self).file_text(file_id)
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish_non_exhaustive()
    }
}

impl salsa::Database for RootDatabase {}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<RootDatabase> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
            parse_servers: self.parse_servers.clone(),
            eqwalizer: self.eqwalizer.clone(),
            eqwalizer_progress_reporter: self.eqwalizer_progress_reporter.clone(),
        })
    }
}

impl RootDatabase {
    pub fn request_cancellation(&mut self) {
        self.salsa_runtime_mut()
            .synthetic_write(salsa::Durability::LOW);
    }

    pub fn clear_parse_servers(&mut self) {
        self.parse_servers.write().unwrap().clear();
    }

    pub fn ensure_parse_server(&self, project_id: ProjectId) -> Result<()> {
        let connection = Connection::start()?;

        let project_data = self.project_data(project_id);
        let path = project_data
            .deps_ebins
            .iter()
            .map(|path| path.clone().into())
            .collect();
        connection.add_code_path(path);

        self.parse_servers
            .write()
            .unwrap()
            .insert(project_id, connection);
        Ok(())
    }

    pub fn set_eqwalizer_progress_reporter(
        &self,
        report: Option<Box<dyn EqwalizerProgressReporter>>,
    ) {
        *self.eqwalizer_progress_reporter.lock().unwrap() = report
    }

    pub fn eqwalizer(&self) -> &Eqwalizer {
        &self.eqwalizer
    }
}

#[salsa::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: SourceDatabase {
    fn file_line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn file_line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(&*text))
}
