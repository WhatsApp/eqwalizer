/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let source_directory = Path::new("../../../eqwalizer");
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let eqwalizer_out_dir = Path::new("../../../eqwalizer/target/scala-2.13");
    let dest_path = Path::new(&out_dir).join("eqwalizer");
    let extension;

    if let Some(path) = env::var_os("ELP_EQWALIZER_PATH") {
        let from = Path::new(&path);
        extension = from
            .extension()
            .unwrap_or_default()
            .to_str()
            .unwrap()
            .to_string();
        fs::copy(from, dest_path).expect("Copying precompiled eqwalizer failed");
    } else {
        extension = "jar".to_string();
        let output = Command::new("sbt")
            .arg("assembly")
            .current_dir(source_directory)
            .output()
            .expect("failed to execute sbt assembly");

        if !output.status.success() {
            let stdout =
                String::from_utf8(output.stdout).expect("valid utf8 output from sbt assembly");
            let stderr =
                String::from_utf8(output.stderr).expect("valid utf8 output from sbt assembly");
            panic!(
                "sbt assembly failed with stdout:\n{}\n\nstderr:\n{}",
                stdout, stderr
            );
        }

        let from = eqwalizer_out_dir.join("eqwalizer.jar");
        fs::copy(from, dest_path).expect("Copying fresh eqwalizer failed");

        rerun_if_changed(source_directory.join("build.sbt"));
        rerun_if_changed(source_directory.join("src"));
    }

    println!("cargo:rustc-env=ELP_EQWALIZER_EXT={}", extension);
    println!("cargo:rerun-if-env-changed=ELP_EQWALIZER_PATH");
}

fn rerun_if_changed(path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", path.as_ref().display());
}
