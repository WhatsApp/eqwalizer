/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::borrow::Borrow;
use std::hash::Hash;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_project_model::BuildInfo;
use elp_project_model::Otp;
use elp_project_model::Project;
use elp_project_model::ProjectAppData;
use elp_project_model::RebarProject;
use fxhash::FxHashMap;
use paths::AbsPath;
use paths::RelPath;
use vfs::file_set::FileSet;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;

use crate::SourceDatabaseExt;

/// Files are grouped into source roots. A source root is a directory on the
/// file systems which is watched for changes. Typically it corresponds to an OTP
/// application. Source roots *might* be nested: in this case, a file belongs to
/// the nearest enclosing source root. Paths to files are always relative to a
/// source root, and ELP does not know the root path of the source root at
/// all. So, a file from one source root can't refer to a file in another source
/// root by path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRootId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceRoot {
    pub(crate) file_set: FileSet,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FileSource {
    Src,
    Extra,
}

impl SourceRoot {
    pub fn new(file_set: FileSet) -> SourceRoot {
        SourceRoot { file_set }
    }

    pub fn path_for_file(&self, file: &FileId) -> Option<&VfsPath> {
        self.file_set.path_for_file(file)
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<FileId> {
        self.file_set.file_for_path(path).copied()
    }

    pub fn relative_path(&self, file: FileId, segment: &str) -> Option<FileId> {
        let base_path = self.path_for_file(&file)?;
        self.file_for_path(&base_path.parent()?.join(segment)?)
    }

    pub fn iter(&self) -> impl Iterator<Item = FileId> + '_ {
        self.file_set.iter()
    }

    pub fn iter_app_files<'a>(
        &'a self,
        app_data: &'a AppData,
    ) -> impl Iterator<Item = (FileId, FileSource, &'a VfsPath)> + 'a {
        self.iter().flat_map(move |file_id| {
            let path = self.path_for_file(&file_id)?;

            if app_data.is_extra_src_file(path) {
                Some((file_id, FileSource::Extra, path))
            } else if app_data.is_src_file(path) {
                Some((file_id, FileSource::Src, path))
            } else {
                None
            }
        })
    }
}

/// Source roots (apps) are grouped into projects that share some
/// of the configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProjectId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProjectData {
    pub source_roots: Vec<SourceRootId>,
    pub root_dir: AbsPathBuf,
    pub deps_ebins: Vec<AbsPathBuf>,
    pub build_info_path: Option<AbsPathBuf>,
    pub otp: Otp,
    pub app_roots: AppRoots,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AppData {
    pub project_id: ProjectId,
    pub dir: AbsPathBuf,
    pub include_path: Vec<AbsPathBuf>,
    pub src_dirs: Vec<String>,
    pub extra_src_dirs: Vec<String>,
    pub macros: Vec<eetf::Term>,
    pub parse_transforms: Vec<eetf::Term>,
    pub app_type: AppType,
}

impl AppData {
    pub fn add_include_path(mut self, path: AbsPathBuf) -> AppData {
        self.include_path.push(path);
        self
    }

    fn is_src_file(&self, path: &VfsPath) -> bool {
        if let Some(path) = self.local_file_path(path) {
            // src_dirs are recursive, check path begins with one
            return self
                .src_dirs
                .iter()
                .any(|src_dir| path.as_ref().starts_with(src_dir));
        }
        false
    }

    fn is_extra_src_file(&self, path: &VfsPath) -> bool {
        if let Some(path) = self.local_file_path(path) {
            // extra_src_dirs are not recursive, check parent dir is one
            if let Some(parent) = path.as_ref().parent() {
                return self
                    .extra_src_dirs
                    .iter()
                    .any(|src_dir| parent == Path::new(src_dir));
            }
        }
        false
    }

    fn local_file_path<'a>(&self, path: &'a VfsPath) -> Option<&'a RelPath> {
        path.as_path()?.strip_prefix(&self.dir)
    }
}

/// Note that `AppStructure` is build-system agnostic
#[derive(Debug, Clone, Default /* Serialize, Deserialize */)]
pub struct AppStructure {
    pub(crate) app_map: FxHashMap<SourceRootId, Option<AppData>>,
    pub(crate) project_map: FxHashMap<ProjectId, ProjectData>,
}

impl AppStructure {
    pub fn add_app_data(&mut self, source_root_id: SourceRootId, app_data: Option<AppData>) {
        let prev = self.app_map.insert(source_root_id, app_data);
        assert!(prev.is_none());
    }
    pub fn add_project_data(&mut self, project_id: ProjectId, project_data: ProjectData) {
        let prev = self.project_map.insert(project_id, project_data);
        assert!(prev.is_none());
    }
    /// If the source_root_id has an entry in the graph return it, else return
    /// an empty list.
    pub fn include_path(&self, source_root_id: SourceRootId) -> Vec<AbsPathBuf> {
        match self.app_map.get(&source_root_id) {
            Some(Some(app_data)) => app_data.include_path.clone(),
            _ => vec![],
        }
    }

    /// Set the salsa inputs according to this AppStructure
    pub fn apply(self, db: &mut dyn SourceDatabaseExt) {
        for (source_root_id, data) in self.app_map {
            let arc_data = data.map(Arc::new);
            db.set_app_data(source_root_id, arc_data);
        }
        for (project_id, project_data) in self.project_map {
            db.set_project_data(project_id, Arc::new(project_data));
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AppRoots {
    app_map: FxHashMap<AppName, SourceRootId>,
}

impl AppRoots {
    pub fn new(map: FxHashMap<AppName, SourceRootId>) -> AppRoots {
        AppRoots { app_map: map }
    }

    pub fn insert(&mut self, app: AppName, source_root_id: SourceRootId) {
        self.app_map.insert(app, source_root_id);
    }

    pub fn get<Q: ?Sized>(&self, app: &Q) -> Option<SourceRootId>
    where
        AppName: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.app_map.get(app).cloned()
    }

    pub fn merge(&mut self, other: AppRoots) {
        self.app_map.extend(other.app_map);
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeOtp {
    Yes,
    No,
}

#[derive(Debug)]
pub struct ProjectApps<'a> {
    /// All the applications in a set of projects.  The order here
    /// will correspond with the vfs sourceRootId's
    pub all_apps: Vec<(ProjectId, &'a ProjectAppData)>,
    pub otp_project_id: Option<ProjectId>,
    // We store the original projects to we can make the AppStructure later
    projects: Vec<Project>,
}

impl<'a> ProjectApps<'a> {
    pub fn new(projects: &'a [Project], include_otp: IncludeOtp) -> ProjectApps<'a> {
        let mut all_apps: Vec<(ProjectId, &ProjectAppData)> = projects
            .iter()
            .enumerate()
            .map(|(project_idx, project)| {
                project
                    .rebar
                    .apps
                    .iter()
                    .chain(project.rebar.deps.iter())
                    .map(move |p| (ProjectId(project_idx as u32), p))
            })
            .flatten()
            .collect();

        // We assume that all of the `Project`s use the same OTP.
        // And so we treat the very first one as another
        // `RebarProject`, but for OTP.
        let otp = &projects[0].otp;
        let mut projects: Vec<_> = projects.into();
        let otp_project_id = if include_otp == IncludeOtp::Yes {
            let otp_project_id = ProjectId(projects.len() as u32);
            let mut all_otp_apps: Vec<(ProjectId, &ProjectAppData)> =
                otp.apps.iter().map(|app| (otp_project_id, app)).collect();
            all_apps.append(&mut all_otp_apps);
            // The only part of this we (currently) use in
            // ProjectRootMap::app_structure() is Project.otp
            let otp_project = Project {
                build_info: BuildInfo::Otp,
                otp: otp.clone(),
                rebar: RebarProject {
                    apps: vec![],
                    deps: vec![],
                    root: AbsPath::assert(&PathBuf::from("/")).normalize(),
                },
            };
            projects.push(otp_project);
            Some(otp_project_id)
        } else {
            None
        };

        ProjectApps {
            all_apps,
            otp_project_id,
            projects,
        }
    }

    pub fn app_structure(&self) -> AppStructure {
        let mut app_structure = AppStructure::default();
        let mut app_idx = 0;

        // Reconstruct the per-project list
        let mut apps_by_project: FxHashMap<ProjectId, Vec<&ProjectAppData>> = FxHashMap::default();

        for (project_id, appdata) in self.all_apps.iter() {
            apps_by_project
                .entry(*project_id)
                .or_default()
                .push(appdata);
        }

        for (project_id, apps) in apps_by_project.iter() {
            let mut project_source_roots = vec![];

            for app in apps.iter() {
                let root_id = SourceRootId(app_idx);
                app_idx += 1;
                project_source_roots.push(root_id);
                let mut include_path = app.include_dirs();
                let project = &self.projects[project_id.0 as usize];
                let global_includes = project.global_includes();
                include_path.extend(global_includes.iter().cloned());

                let input_data = AppData {
                    project_id: *project_id,
                    dir: app.dir.clone(),
                    include_path,
                    src_dirs: app.src_dirs.clone(),
                    extra_src_dirs: app.extra_src_dirs.clone(),
                    macros: app.macros.clone(),
                    parse_transforms: app.parse_transforms.clone(),
                    app_type: app.app_type,
                };
                app_structure.add_app_data(root_id, Some(input_data));
            }
            let project = &self.projects[project_id.0 as usize];
            let project_data = ProjectData {
                source_roots: project_source_roots,
                root_dir: project.rebar.root.clone(),
                deps_ebins: project.deps_ebins(),
                build_info_path: project.build_info_file(),
                otp: project.otp.clone(),
                app_roots: self.app_roots(*project_id),
            };
            app_structure.add_project_data(*project_id, project_data);
        }

        // Final SourceRoot for out-of-project files
        log::info!("Final source root: {:?}", SourceRootId(app_idx));
        app_structure.add_app_data(SourceRootId(app_idx), None);
        app_structure
    }

    fn app_source_roots(
        all_apps: &Vec<(ProjectId, &ProjectAppData)>,
    ) -> FxHashMap<ProjectId, AppRoots> {
        let mut app_source_roots: FxHashMap<ProjectId, AppRoots> = FxHashMap::default();

        for (idx, (project_id, app)) in all_apps.iter().enumerate() {
            let source_root_id = SourceRootId(idx as u32);
            app_source_roots
                .entry(*project_id)
                .or_default()
                .insert(app.name.clone(), source_root_id);
        }
        app_source_roots
    }

    pub fn app_roots(&self, project_id: ProjectId) -> AppRoots {
        let project_root_map = ProjectApps::app_source_roots(&self.all_apps);
        let mut app_roots = project_root_map
            .get(&project_id)
            .unwrap_or(&AppRoots::new(FxHashMap::default()))
            .clone();
        // This leads to duplicating the OTP AppRoots in every
        // project.  It is a small amount of data, and there are not a
        // large number of projects, so it is probably OK.
        if let Some(otp_project_id) = self.otp_project_id {
            let otp_app_roots = project_root_map
                .get(&otp_project_id)
                .unwrap_or(&AppRoots::new(FxHashMap::default()))
                .clone();
            app_roots.merge(otp_app_roots);
        }
        app_roots
    }
}
