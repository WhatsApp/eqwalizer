/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Forms.OverloadedFunSpec
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.Types.{DynamicType, FunType, NoneType, Type}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.NoSpecialType

class ElabApplyOverloaded(pipelineContext: PipelineContext) {
  private lazy val check = pipelineCtx.check
  private lazy val elab = pipelineContext.elab
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype
  private val elabApply = pipelineContext.elabApply
  private val narrow = pipelineContext.narrow
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def elabOverloaded(expr: Expr, remoteId: RemoteId, args: List[Expr], env: Env): (Type, Env) = {
    val depFunSpec = util.getOverloadedSpec(remoteId).get
    val (argTys, env1) = elab.elabExprs(args, env)
    selectTypes(depFunSpec, argTys) match {
      case List(ft: FunType) =>
        val resTy = elabApply.elabApply(check.freshen(ft), args, argTys, env1)
        (resTy, env1)
      case _ =>
        if (pipelineCtx.overloadedSpecDynamicResult)
          diagnosticsInfo.add(NoSpecialType(expr.pos, expr, argTys))
        (DynamicType, env1)
    }
  }

  def isOverloadedFun(remoteId: RemoteId): Boolean =
    util.getOverloadedSpec(remoteId).isDefined

  private def mightOverlap(t1: Type, t2: Type): Boolean = {
    val approxMeet = narrow.meet(t1, t2)
    !subtype.subType(approxMeet, NoneType)
  }

  private def selectTypes(depFunSpec: OverloadedFunSpec, argTys: List[Type]): List[FunType] =
    depFunSpec.tys.filter(ft => argTys.lazyZip(ft.argTys).forall(mightOverlap))
}
