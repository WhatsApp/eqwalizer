/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp

import com.typesafe.config.ConfigFactory

package object eqwalizer {
  object Mode {
    sealed trait Mode

    case object Shell extends Mode

    case object ElpCli extends Mode

    case object ElpIde extends Mode

    def fromString(str: String): Option[Mode] = {
      str match {
        case "shell"   => Some(Shell)
        case "elp_cli" => Some(ElpCli)
        case "elp_ide" => Some(ElpIde)
        case _         => None
      }
    }
  }

  case class Config(
      codeWidth: Int,
      astDir: Option[String],
      approximateComplexTypes: Boolean,
      eqwater: Boolean,
      tolerateErrors: Boolean,
      checkRedundantGuards: Boolean,
      clauseCoverage: Boolean,
      overloadedSpecDynamicResult: Boolean,
      customMapsMerge: Boolean,
      mode: Mode.Mode,
      errorDepth: Int,
  )

  lazy val config: Config = {
    val config = ConfigFactory.load().getConfig("eqwalizer")
    val modeStr = config.getString("mode")
    val mode = Mode.fromString(modeStr).getOrElse(throw new IllegalArgumentException(s"Unknown mode ${modeStr}"))
    Config(
      codeWidth = config.getInt("code_width"),
      astDir = if (config.hasPath("ast_dir")) Some(config.getString("ast_dir")) else None,
      approximateComplexTypes = config.getBoolean("approximate_complex_types"),
      eqwater = config.getBoolean("eqwater"),
      tolerateErrors = config.getBoolean("tolerate_errors"),
      checkRedundantGuards = config.getBoolean("check_redundant_guards"),
      clauseCoverage = config.getBoolean("clause_coverage"),
      overloadedSpecDynamicResult = config.getBoolean("overloaded_spec_dynamic_result"),
      customMapsMerge = config.getBoolean("custom_maps_merge"),
      mode,
      errorDepth = config.getInt("error_depth"),
    )
  }
}
