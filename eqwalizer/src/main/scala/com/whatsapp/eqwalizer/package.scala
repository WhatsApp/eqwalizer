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
  object Mode {
    sealed trait Mode

    case object Standalone extends Mode

    case object Shell extends Mode

    case object ElpCli extends Mode

    case object ElpIde extends Mode

    def fromString(str: String): Option[Mode] = {
      str match {
        case "standalone" => Some(Standalone)
        case "shell"      => Some(Shell)
        case "elp_cli"    => Some(ElpCli)
        case "elp_ide"    => Some(ElpIde)
        case _            => None
      }
    }
  }

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
      tolerateErrors: Boolean,
      checkRedundantGuards: Boolean,
      clauseCoverage: Boolean,
      overloadedSpecDynamicResult: Boolean,
      mode: Mode.Mode,
      errorDepth: Int,
  ) {
    def useElp(): Boolean = {
      mode match {
        case Mode.Shell | Mode.ElpCli | Mode.ElpIde => true
        case Mode.Standalone                        => false
      }
    }
  }

  lazy val config: Config = {
    val config = ConfigFactory.load().getConfig("eqwalizer")
    val buildInfoPath = config.getString("build_info")
    val buildInfo = BuildInfo.load(buildInfoPath)
    val modeStr = config.getString("mode")
    val mode = Mode.fromString(modeStr).getOrElse(throw new IllegalArgumentException(s"Unknown mode ${modeStr}"))
    Config(
      otpLibRoot = buildInfo.otpLibRoot,
      sourceRoot = buildInfo.sourceRoot,
      apps = buildInfo.apps,
      deps = buildInfo.deps,
      depApps = buildInfo.deps.keySet,
      codeWidth = config.getInt("code_width"),
      astDir = if (config.hasPath("ast_dir")) Some(config.getString("ast_dir")) else None,
      gradualTyping = config.getBoolean("gradual_typing"),
      approximateComplexTypes = config.getBoolean("approximate_complex_types"),
      eqwater = config.getBoolean("eqwater"),
      tolerateErrors = config.getBoolean("tolerate_errors"),
      checkRedundantGuards = config.getBoolean("check_redundant_guards"),
      clauseCoverage = config.getBoolean("clause_coverage"),
      overloadedSpecDynamicResult = config.getBoolean("overloaded_spec_dynamic_result"),
      mode,
      errorDepth = config.getInt("error_depth"),
    )
  }
}
