/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::convert::TryInto;
use std::fmt;
use std::fs;
use std::iter;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use fxhash::FxHashSet;
use lazy_static::lazy_static;
use paths::AbsPath;
use paths::AbsPathBuf;
use tempfile::NamedTempFile;
use tempfile::TempPath;
use walkdir::WalkDir;

/// corresponds to rebar profile
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Profile(pub String);

impl Default for Profile {
    fn default() -> Self {
        Self("test".to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct RebarConfig {
    pub config_file: AbsPathBuf, // rebar config or build_info
    pub profile: Profile,
}

impl RebarConfig {
    pub fn rebar3_command(&self) -> CommandProxy<'_> {
        lazy_static! {
            static ref REBAR_GLOBAL_LOCK: Mutex<()> = Mutex::new(());
        }
        let guard = REBAR_GLOBAL_LOCK.lock().unwrap();
        let mut cmd = Command::new("rebar3");
        cmd.arg("as");
        cmd.arg(&self.profile.0);
        if let Some(parent) = self.config_file.parent() {
            cmd.current_dir(parent);
        }
        CommandProxy(guard, cmd)
    }
}

pub struct CommandProxy<'a>(MutexGuard<'a, ()>, Command);

impl<'a> Deref for CommandProxy<'a> {
    type Target = Command;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a> DerefMut for CommandProxy<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ProjectManifest {
    RebarConfig(RebarConfig),
    OTP,
}

impl ProjectManifest {
    pub fn from_manifest_file(rebar_config: RebarConfig) -> Result<ProjectManifest> {
        let path = &rebar_config.config_file;
        if path_ends_with(path, "rebar.config") || path_ends_with(path, "rebar.config.script") {
            Ok(ProjectManifest::RebarConfig(rebar_config))
        } else {
            bail!(
                "project root must point to rebar.config or rebar.config.script: {}",
                path.display()
            )
        }
    }

    pub fn discover_single(path: &AbsPath, project_profile: &Profile) -> Result<ProjectManifest> {
        let mut candidates = ProjectManifest::discover(path, project_profile)?;
        let res = match candidates.pop() {
            None => bail!("no projects"),
            Some(it) => it,
        };

        if !candidates.is_empty() {
            bail!("more than one project")
        }

        Ok(res)
    }

    pub fn discover(path: &AbsPath, project_profile: &Profile) -> Result<Vec<ProjectManifest>> {
        const TARGETS: &[&str] = &["rebar.config", "rebar.config.script"];

        let paths = match find_in_parent_dirs(path, TARGETS) {
            Some(it) => vec![it],
            None => find_in_child_dirs(path, TARGETS)?,
        };

        return Ok(paths
            .into_iter()
            .map(|path| {
                ProjectManifest::from_manifest_file(RebarConfig {
                    config_file: path,
                    profile: project_profile.clone(),
                })
                .unwrap()
            })
            .collect());

        fn find_in_parent_dirs(path: &AbsPath, target_files: &[&str]) -> Option<AbsPathBuf> {
            if target_files.iter().any(|file| path_ends_with(path, file)) {
                return Some(path.to_path_buf());
            }

            let mut curr: Option<&Path> = Some(path.as_ref());
            let ancestors = iter::from_fn(|| {
                let next = curr.and_then(|path| path.parent());
                mem::replace(&mut curr, next)
            });

            ancestors
                .flat_map(|path| find_in_dir(path, target_files))
                .next()
        }

        fn find_in_child_dirs(path: &AbsPath, target_files: &[&str]) -> Result<Vec<AbsPathBuf>> {
            let mut result = vec![];

            let mut iter = WalkDir::new(path).max_depth(2).into_iter();
            while let Some(entry) = iter.next() {
                let entry = entry?;
                let path = entry.path();
                // TODO (T104357707): this is an ugly hack and should be cleaned up
                if path.ends_with("third-party/vendor")
                    || path.ends_with("erlang_ls")
                    || path.ends_with("fbpkg-dependencies")
                {
                    iter.skip_current_dir();
                } else if let Some(found) = find_in_dir(path, target_files) {
                    iter.skip_current_dir();
                    result.push(found)
                }
            }

            Ok(result)
        }

        fn find_in_dir(path: &Path, target_files: &[&str]) -> Option<AbsPathBuf> {
            target_files
                .into_iter()
                .map(move |file| path.join(file))
                .find(|file| file.exists())
                .map(AbsPathBuf::assert)
        }
    }

    pub fn discover_all(
        paths: &[impl AsRef<AbsPath>],
        project_setting: &Profile,
    ) -> Vec<ProjectManifest> {
        let mut res = paths
            .iter()
            .filter_map(|it| ProjectManifest::discover(it.as_ref(), project_setting).ok())
            .flatten()
            .collect::<FxHashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        res.sort();
        res
    }
}

#[derive(Clone)]
pub struct Project {
    pub build_info: BuildInfo,
    pub otp: Otp,
    pub rebar: RebarProject,
}

#[derive(Clone)]
pub enum BuildInfo {
    Otp,
    Cached(RebarConfig, AbsPathBuf),
    Loaded(RebarConfig, Arc<TempPath>),
}

impl PartialEq for Project {
    fn eq(&self, other: &Self) -> bool {
        // Explicitly ignore build_info field - if we inferred the same data after loading
        // from rebar, it's enough for equality comparison
        self.otp == other.otp && self.rebar == other.rebar
    }
}

impl Project {
    pub fn add_app(&mut self, app: ProjectAppData) {
        self.rebar.apps.push(app);
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.rebar
            .root
            .file_name()
            .map(|name| name.to_string_lossy())
            .unwrap_or_else(|| self.rebar.root.as_os_str().to_string_lossy())
    }

    pub fn build_info_file(&self) -> Option<AbsPathBuf> {
        match &self.build_info {
            BuildInfo::Otp => None,
            BuildInfo::Cached(_, cached) => Some(cached.clone()),
            BuildInfo::Loaded(_, loaded) => Some(AbsPathBuf::assert(loaded.to_path_buf())),
        }
    }

    pub fn deps_ebins(&self) -> Vec<AbsPathBuf> {
        self.rebar.deps.iter().map(|app| app.ebin.clone()).collect()
    }

    /// Replicates behaviour of -include_lib through
    /// the -include fallback in a regularly structured
    /// rebar3 project without compiling modules
    /// This one includes the OTP include dir too.
    pub fn global_includes(&self) -> Vec<AbsPathBuf> {
        let mut includes = self.rebar.global_includes();
        includes.push(self.otp.lib_dir.clone());
        includes
    }
}

impl fmt::Debug for Project {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Project")
            .field("otp", &self.otp)
            .field("rebar", &self.rebar)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RebarProject {
    pub apps: Vec<ProjectAppData>,
    pub deps: Vec<ProjectAppData>,
    pub root: AbsPathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppName(pub String);

impl Borrow<str> for AppName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AppType {
    App,
    Dep,
    Otp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectAppData {
    pub name: AppName,
    pub dir: AbsPathBuf,
    pub ebin: AbsPathBuf,
    pub src_dirs: Vec<String>,
    pub extra_src_dirs: Vec<String>,
    pub include_dirs: Vec<AbsPathBuf>,
    pub macros: Vec<eetf::Term>,
    pub parse_transforms: Vec<eetf::Term>,
    pub app_type: AppType,
}

impl ProjectAppData {
    pub fn fixture_app_data(
        name: AppName,
        dir: AbsPathBuf,
        include_dirs: Vec<AbsPathBuf>,
    ) -> ProjectAppData {
        ProjectAppData {
            name,
            ebin: dir.join("ebin"),
            src_dirs: vec!["src".to_string()],
            extra_src_dirs: vec![],
            include_dirs,
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::App,
        }
    }

    pub fn otp_app_data(versioned_name: &str, dir: AbsPathBuf) -> Self {
        let name = versioned_name
            .split_once('-')
            .map_or(versioned_name, |(base, _version)| base);

        Self {
            name: AppName(name.to_string()),
            ebin: dir.join("ebin"),
            src_dirs: vec!["src".to_string()],
            extra_src_dirs: vec![],
            // This makes sure files in ./include are loaded into VFS
            include_dirs: vec![dir.join("include")],
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::Otp,
        }
    }

    pub fn include_dirs(&self) -> Vec<AbsPathBuf> {
        self.include_dirs
            .iter()
            .cloned()
            .chain(self.src_dirs.iter().map(|src_dir| self.dir.join(src_dir)))
            .collect()
    }

    /// Source directories for the application including the extra
    /// sources and includes
    pub fn all_source_dirs(&self) -> Vec<AbsPathBuf> {
        self.src_dirs
            .iter()
            .chain(self.extra_src_dirs.iter())
            .map(|src_dir| self.dir.join(src_dir))
            .chain(self.include_dirs.iter().cloned())
            .collect()
    }

    /// Combine the info from the other ProjectAppData into this one
    pub fn combine(&mut self, other: ProjectAppData) {
        self.src_dirs.extend(other.src_dirs);
        self.src_dirs.dedup();
        self.extra_src_dirs.extend(other.extra_src_dirs);
        self.extra_src_dirs.dedup();
        self.include_dirs.extend(other.include_dirs);
        self.include_dirs.dedup();
        self.macros.extend(other.macros);
        self.macros.dedup();
        self.parse_transforms.extend(other.parse_transforms);
        self.parse_transforms.dedup();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Otp {
    pub lib_dir: AbsPathBuf,
    pub apps: Vec<ProjectAppData>,
}

impl Project {
    pub fn compile_deps(&self) -> Result<()> {
        let build = match &self.build_info {
            BuildInfo::Otp => return Ok(()),
            BuildInfo::Cached(build, _) => build,
            BuildInfo::Loaded(build, _) => build,
        };

        let mut cmd = build.rebar3_command();
        cmd.arg("compile");
        cmd.arg("--deps_only");

        let _ = utf8_stdout(&mut cmd)?;

        Ok(())
    }

    pub fn load(manifest: ProjectManifest) -> Result<Project> {
        let (manifest, out_file) = match manifest {
            ProjectManifest::OTP => {
                unreachable!(
                    "We cannot load OTP, because we do not know its structure (no build info)"
                )
            }
            ProjectManifest::RebarConfig(rebar_setting) => {
                let rebar_version = {
                    let mut cmd = Command::new("rebar3");
                    cmd.arg("version");
                    utf8_stdout(&mut cmd)?
                };

                let loaded = Project::load_rebar_build_info(&rebar_setting).with_context(|| {
                    format!(
                        "Failed to read rebar build info for config file {}, {}",
                        rebar_setting.config_file.display(),
                        rebar_version
                    )
                })?;
                (rebar_setting, loaded)
            }
        };

        let (rebar, otp_root) =
            RebarProject::from_rebar_build_info(&out_file).with_context(|| {
                format!(
                    "Failed to decode rebar build info for config file {:?}",
                    manifest
                )
            })?;

        Ok(Project {
            build_info: BuildInfo::Loaded(manifest, Arc::new(out_file)),
            otp: Otp::discover(otp_root),
            rebar,
        })
    }

    pub fn load_cached(manifest: ProjectManifest, build_info: AbsPathBuf) -> Result<Project> {
        let manifest = match manifest {
            ProjectManifest::OTP => {
                unreachable!(
                    "We cannot load OTP, because we do not know its structure (no build info)"
                )
            }
            ProjectManifest::RebarConfig(rebar_setting) => rebar_setting,
        };

        let (rebar, otp_root) =
            RebarProject::from_rebar_build_info(&build_info).with_context(|| {
                format!(
                    "Failed to decode rebar build info for config file {:?}",
                    manifest
                )
            })?;

        Ok(Project {
            build_info: BuildInfo::Cached(manifest, build_info),
            otp: Otp::discover(otp_root),
            rebar,
        })
    }

    fn load_rebar_build_info(build: &RebarConfig) -> Result<TempPath> {
        let out_file = NamedTempFile::new()?.into_temp_path();
        let mut cmd = build.rebar3_command();
        cmd.arg("build_info");
        cmd.arg("--to");
        cmd.arg(&out_file);

        let _ = utf8_stdout(&mut cmd)?;

        Ok(out_file)
    }
}

impl RebarProject {
    pub fn from_rebar_build_info(path: impl AsRef<Path>) -> Result<(RebarProject, PathBuf)> {
        Self::_from_rebar_build_info(path.as_ref())
    }

    fn _from_rebar_build_info(path: &Path) -> Result<(RebarProject, PathBuf)> {
        let data = fs::read(path)?;
        let build_info = eetf::Term::decode(&*data)?;

        return Ok((
            RebarProject {
                apps: to_vec(map_get(&build_info, "apps")?)?
                    .into_iter()
                    .map(|term| to_app_data(term, AppType::App))
                    .collect::<Result<_>>()?,
                deps: to_vec(map_get(&build_info, "deps")?)?
                    .into_iter()
                    .map(|term| to_app_data(term, AppType::Dep))
                    .collect::<Result<_>>()?,
                root: to_abs_path(map_get(&build_info, "source_root")?)?,
            },
            to_string(map_get(&build_info, "otp_lib_dir")?)?.into(),
        ));

        fn to_app_data(term: &eetf::Term, is_dep: AppType) -> Result<ProjectAppData> {
            Ok(ProjectAppData {
                name: AppName(to_string(map_get(term, "name")?)?.to_string()),
                dir: to_abs_path(map_get(term, "dir")?)?,
                ebin: to_abs_path(map_get(term, "ebin")?)?,
                src_dirs: to_vec(map_get(term, "src_dirs")?)?
                    .into_iter()
                    .map(|term| Ok(to_string(term)?.to_owned()))
                    .collect::<Result<_>>()?,
                extra_src_dirs: to_vec(map_get(term, "extra_src_dirs")?)?
                    .into_iter()
                    .map(|term| Ok(to_string(term)?.to_owned()))
                    .collect::<Result<_>>()?,
                include_dirs: to_vec(map_get(term, "include_dirs")?)?
                    .into_iter()
                    .map(to_abs_path)
                    .collect::<Result<_>>()?,
                macros: to_vec(map_get(term, "macros")?)?.to_owned(),
                parse_transforms: to_vec(map_get(term, "parse_transforms")?)?.to_owned(),
                app_type: is_dep,
            })
        }
    }

    /// Replicates behaviour of -include_lib through
    /// the -include fallback in a regularly structured
    /// rebar3 project without compiling modules
    pub fn global_includes(&self) -> Vec<AbsPathBuf> {
        self.apps
            .iter()
            .chain(&self.deps)
            .filter_map(|app| app.dir.parent())
            .map(AbsPath::to_path_buf)
            .collect::<FxHashSet<_>>()
            .into_iter()
            .collect()
    }
}

impl Otp {
    pub fn discover(path: PathBuf) -> Otp {
        let apps = Self::discover_otp_apps(&path);
        Otp {
            lib_dir: AbsPathBuf::assert(path),
            apps,
        }
    }

    fn discover_otp_apps(path: &Path) -> Vec<ProjectAppData> {
        log::info!("Loading OTP apps from {:?}", path);
        if let Ok(entries) = fs::read_dir(path) {
            entries
                .into_iter()
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let name = entry.file_name();
                    let dir = AbsPathBuf::assert(entry.path());
                    Some(ProjectAppData::otp_app_data(name.to_str()?, dir))
                })
                .collect()
        } else {
            vec![]
        }
    }

    /// Used to combine more than one OTP definition in a test suite
    pub fn combine(&mut self, other: Otp) {
        self.apps.extend(other.apps);
    }
}

fn path_ends_with(path: &AbsPath, ending: impl AsRef<Path>) -> bool {
    path.ends_with(&paths::RelPath::new_unchecked(ending.as_ref()))
}

pub fn utf8_stdout(cmd: &mut Command) -> Result<String> {
    let output = cmd.output().with_context(|| format!("{:?} failed", cmd))?;
    let stdout = String::from_utf8(output.stdout)?;
    if !output.status.success() {
        match String::from_utf8(output.stderr) {
            Ok(stderr) if !stderr.is_empty() => {
                bail!(
                    "{:?} failed, {}\nstdout:\n{}\nstderr:\n{}",
                    cmd,
                    output.status,
                    stdout,
                    stderr
                )
            }
            _ => bail!("{:?} failed, {}\nstdout:{}", cmd, output.status, stdout),
        }
    }
    Ok(stdout.trim().to_string())
}

fn map_get<'a>(term: &'a eetf::Term, key: &str) -> Result<&'a eetf::Term> {
    let expected = eetf::Atom::from(key).into();
    match term {
        eetf::Term::Map(eetf::Map { entries }) => entries
            .into_iter()
            .find_map(|(key, value)| if key == &expected { Some(value) } else { None })
            .ok_or(anyhow!("missing key {:?}", key)),
        _ => bail!("expected a map, got: {:?}", term),
    }
}

fn to_bin(term: &eetf::Term) -> Result<&[u8]> {
    match term {
        eetf::Term::Binary(eetf::Binary { bytes }) => Ok(bytes),
        _ => bail!("expected a binary, got: {:?}", term),
    }
}

fn to_string(term: &eetf::Term) -> Result<&str> {
    Ok(std::str::from_utf8(to_bin(term)?)?)
}

fn to_abs_path(term: &eetf::Term) -> Result<AbsPathBuf> {
    match PathBuf::from(to_string(term)?).try_into() {
        Ok(abs_path) => Ok(abs_path),
        Err(path_buf) => bail!("expected absolute path, got: {:?}", path_buf),
    }
}

fn to_vec(term: &eetf::Term) -> Result<&[eetf::Term]> {
    match term {
        eetf::Term::List(eetf::List { elements }) => Ok(elements),
        _ => bail!("expected a list, got: {:?}", term),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover() {
        let root = AbsPathBuf::assert(Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures"));
        let project_profile = Default::default();
        let manifest = ProjectManifest::discover(&root, &project_profile).unwrap();
        assert_eq!(manifest.len(), 1);
        match &manifest[0] {
            ProjectManifest::OTP => {
                unreachable!()
            }
            ProjectManifest::RebarConfig(RebarConfig {
                config_file: actual,
                profile: _,
            }) => {
                let expected = root.join("nested").join("rebar.config.script");
                assert_eq!(actual, &expected);
            }
        }
    }
}
