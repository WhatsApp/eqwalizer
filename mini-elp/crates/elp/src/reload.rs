/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::iter;

use elp_ide_db::elp_base_db::loader;
use elp_ide_db::elp_base_db::AppRoots;
use elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide_db::elp_base_db::ProjectApps;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::elp_base_db::SourceRootId;
use elp_ide_db::elp_base_db::VfsPath;
use fxhash::FxHashMap;

#[derive(Debug)]
pub struct ProjectFolders {
    pub load: Vec<loader::Entry>,
    pub watch: Vec<lsp_types::FileSystemWatcher>,
    pub file_set_config: FileSetConfig,
}

impl ProjectFolders {
    pub fn new(project_apps: &ProjectApps) -> ProjectFolders {
        let file_set_config = project_apps
            .all_apps
            .iter()
            .fold(
                FileSetConfig::builder(),
                |mut builder, (_project_id, app)| {
                    builder.add_file_set(vec![VfsPath::from(app.dir.clone())]);
                    builder
                },
            )
            .build();

        let mut app_source_roots: FxHashMap<ProjectId, AppRoots> = FxHashMap::default();

        for (idx, (project_id, app)) in project_apps.all_apps.iter().enumerate() {
            let source_root_id = SourceRootId(idx as u32);
            app_source_roots
                .entry(*project_id)
                .or_default()
                .insert(app.name.clone(), source_root_id);
        }

        let load = project_apps
            .all_apps
            .iter()
            .map(|(_, app)| {
                let dirs = loader::Directories {
                    extensions: vec!["erl".to_string(), "hrl".to_string()],
                    include: app.all_source_dirs(),
                    exclude: vec![],
                };
                loader::Entry::Directories(dirs)
            })
            .collect();

        let watch = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| iter::repeat(project_id).zip(app.all_source_dirs()))
            .filter_map(|(project_id, root)| {
                if Some(*project_id) != project_apps.otp_project_id {
                    Some(lsp_types::FileSystemWatcher {
                        glob_pattern: format!("{}/**/*.{{e,h}}rl", root.display()),
                        kind: None,
                    })
                } else {
                    None
                }
            })
            .collect();

        ProjectFolders {
            load,
            watch,
            file_set_config,
        }
    }
}
