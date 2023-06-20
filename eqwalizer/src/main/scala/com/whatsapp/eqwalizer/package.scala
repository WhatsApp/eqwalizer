/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp

import com.typesafe.config.ConfigFactory
import com.whatsapp.eqwalizer.io.BuildInfo
import com.whatsapp.eqwalizer.io.BuildInfo.AppInfo

package object eqwalizer {
  case class Config(
      otpLibRoot: String,
      sourceRoot: String,
      apps: Map[String, AppInfo],
      deps: Map[String, AppInfo],
      depApps: Set[String],
      codeWidth: Int,
      astDir: Option[String],
      gradualTyping: Boolean,
      approximateComplexTypes: Boolean,
      eqwater: Boolean,
      useElp: Boolean,
      useIpc: Boolean,
      useElpConvertedAst: Boolean,
      elpShell: Boolean,
      tolerateErrors: Boolean,
      checkRedundantGuards: Boolean,
  )

  lazy val config: Config = {
    val config = ConfigFactory.load().getConfig("eqwalizer")
    val buildInfoPath = config.getString("build_info")
    val buildInfo = BuildInfo.load(buildInfoPath)
    Config(
      otpLibRoot = buildInfo.otpLibRoot,
      sourceRoot = buildInfo.sourceRoot,
      apps = buildInfo.apps,
      deps = buildInfo.deps,
      depApps = buildInfo.deps.keySet,
      codeWidth = config.getInt("code_width"),
      useElp = config.getBoolean("use_elp"),
      astDir = if (config.hasPath("ast_dir")) Some(config.getString("ast_dir")) else None,
      gradualTyping = config.getBoolean("gradual_typing"),
      approximateComplexTypes = config.getBoolean("approximate_complex_types"),
      eqwater = config.getBoolean("eqwater"),
      useIpc = config.getBoolean("use_ipc"),
      useElpConvertedAst = config.getBoolean("use_elp_converted_ast"),
      elpShell = config.getBoolean("elp_shell"),
      tolerateErrors = config.getBoolean("tolerate_errors"),
      checkRedundantGuards = config.getBoolean("check_redundant_guards"),
    )
  }
}
