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
    let source_directory = "../../parse_server";
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_dir = Path::new(&out_dir).join("parse_server");
    fs::create_dir_all(&dest_dir).unwrap();

    if let Some(path) = env::var_os("ELP_PARSE_SERVER_ESCRIPT_PATH") {
        fs::copy(path, dest_dir.join("parse_server"))
            .expect("Copying precompiled parse server escript failed");
    } else {
        let profile = env::var("PROFILE").unwrap();
        let output = Command::new("rebar3")
            .arg("escriptize")
            .env("REBAR_PROFILE", &profile)
            .env("REBAR_BASE_DIR", &dest_dir)
            .current_dir(source_directory)
            .output()
            .expect("failed to execute rebar3 escriptize");

        if !output.status.success() {
            let stdout =
                String::from_utf8(output.stdout).expect("valid utf8 output from rebar3 escriptize");
            let stderr =
                String::from_utf8(output.stderr).expect("valid utf8 output from rebar3 escriptize");
            panic!(
                "rebar3 escriptize failed with stdout:\n{}\n\nstderr:\n{}",
                stdout, stderr
            );
        }

        let source = dest_dir.join(profile).join("bin").join("parse_server");
        fs::copy(source, dest_dir.join("parse_server")).unwrap();

        println!("cargo:rerun-if-changed={}/rebar.config", source_directory);
        println!("cargo:rerun-if-changed={}/src", source_directory);
    }

    println!("cargo:rerun-if-env-changed=ELP_PARSE_SERVER_ESCRIPT_PATH");
}
