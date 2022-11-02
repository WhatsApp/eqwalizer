/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

//! Loads a rebar project into a static instance of ELP,
//! without support for incorporating changes
use std::fs;
use std::path::Path;
use std::sync::Arc;

use anyhow::Result;
use crossbeam_channel::unbounded;
use crossbeam_channel::Receiver;
use elp::reload::ProjectFolders;
use elp_ide::elp_ide_db::EqwalizerProgressReporter;
use elp_ide::Analysis;
use elp_ide::AnalysisHost;
use elp_ide_db::elp_base_db::loader;
use elp_ide_db::elp_base_db::loader::Handle;
use elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide_db::elp_base_db::ProjectApps;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::elp_base_db::SourceRoot;
use elp_ide_db::elp_base_db::SourceRootId;
use elp_ide_db::elp_base_db::Vfs;
use elp_project_model::Profile;
use elp_project_model::Project;
use elp_project_model::ProjectManifest;
use indicatif::ProgressBar;

use crate::util;

pub struct LoadResult {
    analysis_host: AnalysisHost,
    pub vfs: Vfs,
    pub project_id: ProjectId,
    pub project: Project,
}

impl LoadResult {
    fn new(analysis_host: AnalysisHost, vfs: Vfs, project_id: ProjectId, project: Project) -> Self {
        LoadResult {
            analysis_host,
            vfs,
            project_id,
            project,
        }
    }

    pub fn with_eqwalizer_progress_bar<R>(
        &self,
        pb: ProgressBar,
        f: impl FnOnce(Analysis) -> R,
    ) -> R {
        struct Reporter(ProgressBar);

        impl EqwalizerProgressReporter for Reporter {
            fn report(&self, done: usize) {
                self.0.set_position(done as u64)
            }
        }

        self.analysis_host
            .raw_database()
            .set_eqwalizer_progress_reporter(Some(Box::new(Reporter(pb))));

        let r = f(self.analysis());

        self.analysis_host
            .raw_database()
            .set_eqwalizer_progress_reporter(None);

        r
    }

    pub fn analysis(&self) -> Analysis {
        self.analysis_host.analysis()
    }
}

pub fn load_json_project_at(json: &Path) -> Result<LoadResult> {
    let json_file = AbsPathBuf::assert(std::env::current_dir()?.join(json)).normalize();
    let manifest = ProjectManifest::from_json_file(json_file);

    let pb = util::spinner("Loading JSON manifest", "Loaded JSON manifest");
    let project = Project::load(manifest)?;
    pb.finish();

    load_project(project)
}

pub fn load_project_at(root: &Path, project_profile: &Profile) -> Result<LoadResult> {
    let root = AbsPathBuf::assert(std::env::current_dir()?.join(root));
    let root = ProjectManifest::discover_single(&root, project_profile)?;

    log::info!("Discovered project: {:?}", root);

    let pb = util::spinner("Loading rebar3 build_info", "Loaded rebar3 build_info");
    let project = Project::load(root)?;
    pb.finish();

    load_project(project)
}

pub fn load_project_with_caching_at(
    root: &Path,
    project_profile: &Profile,
    fast: bool,
) -> Result<LoadResult> {
    let root = AbsPathBuf::assert(std::env::current_dir()?.join(root));
    let manifest = ProjectManifest::discover_single(&root, project_profile)?;

    log::info!("Discovered project: {:?}", root);

    let pb = util::spinner("Loading rebar3 build_info", "Loaded rebar3 build_info");

    let build_info = root.join("_build").join("elp").join("wa.build_info");

    let project = if fast {
        Project::load_cached(manifest, build_info)?
    } else {
        let project = Project::load(manifest)?;
        fs::create_dir_all(build_info.parent().unwrap())?;
        fs::copy(project.build_info_file().unwrap(), build_info)?;
        project
    };

    pb.finish();

    load_project(project)
}

pub fn load_project(project: Project) -> Result<LoadResult> {
    let project_id = ProjectId(0);

    let (sender, receiver) = unbounded();
    let mut vfs = Vfs::default();
    let mut loader = {
        let loader =
            vfs_notify::NotifyHandle::spawn(Box::new(move |msg| sender.send(msg).unwrap()));
        Box::new(loader)
    };

    let projects = [project.clone()];
    let project_apps = ProjectApps::new(&projects);
    let folders = ProjectFolders::new(&project_apps);

    let vfs_loader_config = loader::Config {
        load: folders.load,
        watch: vec![],
        version: 0,
    };
    loader.set_config(vfs_loader_config);

    let analysis_host =
        load_database(&project_apps, &folders.file_set_config, &mut vfs, &receiver)?;
    Ok(LoadResult::new(analysis_host, vfs, project_id, project))
}

fn load_database(
    project_apps: &ProjectApps,
    file_set_config: &FileSetConfig,
    vfs: &mut Vfs,
    receiver: &Receiver<loader::Message>,
) -> Result<AnalysisHost> {
    let mut analysis_host = AnalysisHost::default();
    let db = analysis_host.raw_database_mut();

    let pb = util::progress(0, "Loading applications", "Loaded applications");

    for task in receiver {
        match task {
            loader::Message::Progress {
                n_done, n_total, ..
            } => {
                pb.set_length(n_total as u64);
                pb.set_position(n_done as u64);
                if n_done == n_total {
                    break;
                }
            }
            loader::Message::Loaded { files } => {
                for (path, contents) in files {
                    vfs.set_file_contents(path.into(), contents);
                }
            }
        }
    }

    pb.finish();

    let pb = util::spinner("Seeding database", "Database seeded");

    let mut project_source_roots = vec![];

    let sets = file_set_config.partition(vfs);
    for (idx, set) in sets.into_iter().enumerate() {
        let root_id = SourceRootId(idx as u32);
        project_source_roots.push(root_id);
        for file_id in set.iter() {
            db.set_file_source_root(file_id, root_id);
        }
        let root = SourceRoot::new(set);
        db.set_source_root(root_id, Arc::new(root));
    }

    project_apps.app_structure().apply(db);

    let project_id = ProjectId(0);
    db.ensure_parse_server(project_id)?;
    let changes = vfs.take_changes();
    for file in changes {
        if file.exists() {
            let contents = vfs.file_contents(file.file_id).to_vec();
            match String::from_utf8(contents) {
                Ok(text) => {
                    db.set_file_text(file.file_id, Arc::new(text));
                }
                Err(err) => {
                    // Fall back to lossy latin1 loading of files.
                    // This should only affect files from yaws, and
                    // possibly OTP that are latin1 encoded.
                    let contents = err.into_bytes();
                    let text = contents.into_iter().map(|byte| byte as char).collect();
                    db.set_file_text(file.file_id, Arc::new(text));
                }
            }
        }
    }

    pb.finish();

    Ok(analysis_host)
}
