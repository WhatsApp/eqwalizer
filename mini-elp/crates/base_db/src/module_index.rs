/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::borrow::Borrow;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;

use fxhash::FxHashMap;
use smol_str::SmolStr;

use crate::FileId;
use crate::FileSource;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(SmolStr);

impl ModuleName {
    pub fn new(name: &str) -> Self {
        ModuleName(SmolStr::new(name))
    }

    pub fn as_str(&self) -> &str {
        self
    }
}

impl Deref for ModuleName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ModuleIndex {
    mod2file: FxHashMap<ModuleName, (FileSource, FileId)>,
    file2mod: FxHashMap<FileId, ModuleName>,
}

impl fmt::Debug for ModuleIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ModuleIndex(")?;
        let mut map = f.debug_map();
        for entry in &self.mod2file {
            map.entry(&entry.0, &entry.1);
        }
        map.finish()?;
        f.write_str(")")
    }
}

impl ModuleIndex {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn file_for_module<Q: ?Sized>(&self, name: &Q) -> Option<FileId>
    where
        ModuleName: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.mod2file.get(name).map(|(_source, id)| *id)
    }

    pub fn file_source_for_file(&self, file_id: FileId) -> Option<FileSource> {
        self.file2mod
            .get(&file_id)
            .and_then(|name| self.mod2file.get(name).map(|(source, _id)| *source))
    }

    pub fn module_for_file(&self, file_id: FileId) -> Option<&ModuleName> {
        self.file2mod.get(&file_id)
    }

    pub fn len(&self) -> usize {
        self.mod2file.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ModuleName, FileSource, FileId)> + '_ {
        self.mod2file
            .iter()
            .map(|(name, (source, id))| (name, *source, *id))
    }
}

#[derive(Default)]
pub struct Builder(FxHashMap<ModuleName, (FileSource, FileId)>);

impl Builder {
    pub fn insert(&mut self, file_id: FileId, source: FileSource, name: ModuleName) {
        self.0.insert(name, (source, file_id));
    }

    pub fn build(self) -> ModuleIndex {
        let file2mod = self
            .0
            .iter()
            .map(|(name, (_source, file))| (*file, name.clone()))
            .collect::<FxHashMap<_, _>>();

        ModuleIndex {
            mod2file: self.0,
            file2mod,
        }
    }
}
