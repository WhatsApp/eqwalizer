/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use elp_base_db::AbsPath;
use elp_base_db::AbsPathBuf;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_parse_server::Format;
use elp_parse_server::ParseError;

use crate::fixmes;
use crate::parse_server::CompileOption;
use crate::parse_server::ParseRequest;
use crate::LineIndexDatabase;

pub trait AstLoader {
    fn load_ast(
        &self,
        project_id: ProjectId,
        path: &AbsPath,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        elp_metadata: eetf::Term,
        format: Format,
    ) -> Result<Vec<u8>, Vec<ParseError>>;
}

impl AstLoader for crate::RootDatabase {
    fn load_ast(
        &self,
        project_id: ProjectId,
        path: &AbsPath,
        include_path: &[elp_base_db::AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        elp_metadata: eetf::Term,
        format: Format,
    ) -> Result<Vec<u8>, Vec<ParseError>> {
        let includes = include_path
            .iter()
            .map(|path| path.clone().into())
            .collect();
        let options = vec![
            CompileOption::Includes(includes),
            CompileOption::Macros(macros.to_vec()),
            CompileOption::ParseTransforms(parse_transforms.to_vec()),
            CompileOption::ElpMetadata(elp_metadata),
        ];
        let path = path.to_path_buf().into();
        let req = ParseRequest {
            options,
            path,
            format,
        };

        let parse_server = self.parse_servers.read().unwrap().get(&project_id).cloned();
        parse_server.unwrap().request(req)
    }
}

#[salsa::query_group(ErlAstDatabaseStorage)]
pub trait ErlAstDatabase: SourceDatabase + AstLoader + LineIndexDatabase {
    fn module_ast(
        &self,
        file_id: FileId,
        format: Format,
    ) -> Result<Arc<Vec<u8>>, Arc<Vec<ParseError>>>;
}

fn module_ast(
    db: &dyn ErlAstDatabase,
    file_id: FileId,
    format: Format,
) -> Result<Arc<Vec<u8>>, Arc<Vec<ParseError>>> {
    // We rely on the state of the file system. Let salsa always re-execute this query after changes
    db.salsa_runtime().report_untracked_read();

    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id).unwrap().as_path().unwrap();
    let app_data = if let Some(app_data) = db.app_data(root_id) {
        app_data
    } else {
        let err = ParseError {
            path: path.to_path_buf().into(),
            location: None,
            msg: "Unknown application".to_string(),
        };
        return Err(Arc::new(vec![err]));
    };
    let metadata = elp_metadata(db, file_id).into();
    db.load_ast(
        app_data.project_id,
        path,
        &app_data.include_path,
        &app_data.macros,
        &app_data.parse_transforms,
        metadata,
        format,
    )
    .map(Arc::new)
    .map_err(Arc::new)
}

fn elp_metadata(db: &dyn ErlAstDatabase, file_id: FileId) -> eetf::Term {
    let line_index = db.file_line_index(file_id);
    let file_text = db.file_text(file_id);
    let fixmes = fixmes::fixmes_eetf(&line_index, &file_text);
    // Erlang proplist: [{eqwalizer_fixmes, [Fixme1, Fixme2....]}]
    eetf::List::from(vec![
        eetf::Tuple::from(vec![eetf::Atom::from("eqwalizer_fixmes").into(), fixmes]).into(),
    ])
    .into()
}
