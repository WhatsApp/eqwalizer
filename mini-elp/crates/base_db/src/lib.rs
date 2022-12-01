/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use elp_project_model::AppType;
use text_size::TextRange;
use text_size::TextSize;

mod input;
mod module_index;

// ---------------------------------------------------------------------
// Public API

pub use input::AppData;
pub use input::AppRoots;
pub use input::AppStructure;
pub use input::FileSource;
pub use input::ProjectApps;
pub use input::ProjectData;
pub use input::ProjectId;
pub use input::SourceRoot;
pub use input::SourceRootId;
pub use module_index::ModuleIndex;
pub use module_index::ModuleName;
pub use paths::AbsPath;
pub use paths::AbsPathBuf;
pub use paths::RelPath;
pub use paths::RelPathBuf;
pub use salsa;
pub use vfs::file_set::FileSet;
pub use vfs::file_set::FileSetConfig;
pub use vfs::file_set::FileSetConfigBuilder;
pub use vfs::loader;
pub use vfs::AnchoredPath;
pub use vfs::AnchoredPathBuf;
pub use vfs::ChangeKind;
pub use vfs::ChangedFile;
pub use vfs::FileId;
pub use vfs::Vfs;
pub use vfs::VfsPath;

// ---------------------------------------------------------------------

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[derive(Clone, Copy, Debug)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

pub trait FileLoader {
    /// Text of the file.
    fn file_text(&self, file_id: FileId) -> Arc<String>;
}

/// Database which stores all significant input facts: source code and project
/// model. Everything else in ELP is derived from these queries.
#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: FileLoader + salsa::Database {
    /// Path to a ile, relative to the root of its source root.
    /// Source root of the file.
    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    /// Contents of the source root.
    #[salsa::input]
    fn source_root(&self, id: SourceRootId) -> Arc<SourceRoot>;

    #[salsa::input]
    fn app_data(&self, id: SourceRootId) -> Option<Arc<AppData>>;

    #[salsa::input]
    fn project_data(&self, id: ProjectId) -> Arc<ProjectData>;

    /// Returns a map from module name to FileId of the containing file.
    fn module_index(&self, project_id: ProjectId) -> Arc<ModuleIndex>;

    fn file_app_type(&self, file_id: FileId) -> Option<AppType>;

    fn file_app_name(&self, file_id: FileId) -> Option<String>;
}

fn module_index(db: &dyn SourceDatabase, project_id: ProjectId) -> Arc<ModuleIndex> {
    let mut builder = ModuleIndex::builder();

    for &source_root_id in &db.project_data(project_id).source_roots {
        if let Some(app_data) = db.app_data(source_root_id) {
            let source_root = db.source_root(source_root_id);
            for (file_id, file_source, path) in source_root.iter_app_files(&app_data) {
                if let Some((name, Some("erl"))) = path.name_and_extension() {
                    builder.insert(file_id, file_source, ModuleName::new(name));
                }
            }
        }
    }

    Arc::new(builder.build())
}

fn file_app_type(db: &dyn SourceDatabase, file_id: FileId) -> Option<AppType> {
    let app_data = db.app_data(db.file_source_root(file_id))?;
    Some(app_data.app_type)
}

fn file_app_name(db: &dyn SourceDatabase, file_id: FileId) -> Option<String> {
    let app_data = db.app_data(db.file_source_root(file_id))?;
    app_data
        .dir
        .file_name()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
}

/// We don't want to give HIR knowledge of source roots, hence we extract these
/// methods into a separate DB.
#[salsa::query_group(SourceDatabaseExtStorage)]
pub trait SourceDatabaseExt: SourceDatabase {
    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;
}

/// Silly workaround for cyclic deps between the traits
pub struct FileLoaderDelegate<T>(pub T);

impl<T: SourceDatabaseExt> FileLoader for FileLoaderDelegate<&'_ T> {
    fn file_text(&self, file_id: FileId) -> Arc<String> {
        SourceDatabaseExt::file_text(self.0, file_id)
    }
}
