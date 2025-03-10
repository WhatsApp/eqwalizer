/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Types.Type
import com.whatsapp.eqwalizer.ast.Vars
import com.whatsapp.eqwalizer.tc.generics.{Constraints, Variance}

package object tc {
  type Env = Map[String, Type]
  object Env {
    val empty: Env = Map.empty
  }

  case class Options(unlimitedRefinement: Option[Boolean] = None)

  val noOptions: Options = Options()

  case class PipelineContext(module: String, options: Options = Options()) {
    val tolerateErrors: Boolean = config.tolerateErrors
    val util: Util = new Util(this)
    val vars: Vars = new Vars(this)
    val subtype: Subtype = new Subtype(this)
    val narrow: Narrow = new Narrow(this)
    val constraints: Constraints = new Constraints(this)
    val checkCallback: CheckCallback = new CheckCallback(this)
    val subtypeDetail: SubtypeDetail = new SubtypeDetail(this)
    val typeMismatch: TypeMismatch = new TypeMismatch(this)
    val check: Check =
      new Check(this)
    val elab: Elab =
      new Elab(this)
    val elabApply: ElabApply =
      new ElabApply(this)
    val elabApplyCustom: ElabApplyCustom =
      new ElabApplyCustom(this)
    val elabApplyOverloaded: ElabApplyOverloaded =
      new ElabApplyOverloaded(this)
    val elabGuard: ElabGuard =
      new ElabGuard(this)
    val elabPat: ElabPat =
      new ElabPat(this)
    val occurrence: Occurrence =
      new Occurrence(this)
    val customReturn: CustomReturn =
      new CustomReturn(this)
    val unlimitedRefinement: Boolean = {
      options.unlimitedRefinement.getOrElse(false)
    }
    val variance: Variance =
      new Variance(this)
    val typeInfo: TypeInfo =
      new TypeInfo(this)
    val diagnosticsInfo: DiagnosticsInfo =
      new DiagnosticsInfo()
    val errorDepth: Int = config.errorDepth
    val overloadedSpecDynamicResult: Boolean = config.overloadedSpecDynamicResult
    val customMapsMerge: Boolean = config.customMapsMerge
    val ignoredOverloadedSpec: Boolean = config.ignoredOverloadedSpec
    val reportDynamicLambdas: Boolean = config.reportDynamicLambdas
  }
}
