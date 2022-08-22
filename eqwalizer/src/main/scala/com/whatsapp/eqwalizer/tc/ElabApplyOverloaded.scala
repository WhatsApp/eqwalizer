/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.{Expr, Lambda}
import com.whatsapp.eqwalizer.ast.Forms.OverloadedFunSpec
import com.whatsapp.eqwalizer.ast.{AstListener, RemoteId, Traverse}
import com.whatsapp.eqwalizer.ast.Types.{DynamicType, FunType, NoneType, Type}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{NoSpecialType, NotSupportedLambdaInOverloadedCall}

class ElabApplyOverloaded(pipelineContext: PipelineContext) {
  private lazy val check = pipelineCtx.check
  private lazy val elab = pipelineContext.elab
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype
  private val elabApply = pipelineContext.elabApply
  private val narrow = pipelineContext.narrow
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def elabOverloaded(expr: Expr, remoteId: RemoteId, args: List[Expr], env: Env): (Type, Env) = {
    if (!pipelineCtx.gradualTyping && hasLambda(args)) {
      throw NotSupportedLambdaInOverloadedCall(expr.pos, expr)
    }
    val depFunSpec = util.getOverloadedSpec(remoteId).get
    val (argTys, env1) = elab.elabExprs(args, env)
    selectTypes(depFunSpec, argTys) match {
      case List(ft: FunType) =>
        val resTy = elabApply.elabApply(check.freshen(ft), args, argTys, env1)
        (resTy, env1)
      case _ =>
        if (pipelineCtx.gradualTyping)
          (DynamicType, env1)
        else
          throw NoSpecialType(expr.pos, expr, argTys)
    }
  }

  private def hasLambda(args: List[Expr]): Boolean = {
    val listener = new LambdaListener()
    val traverse = new Traverse(listener)
    args.foreach(traverse.traverseExpr)
    listener.hasLambda
  }

  private class LambdaListener() extends AstListener {
    var hasLambda: Boolean = false
    override def enterExpr(e: Expr): Unit = e match {
      case Lambda(_) => hasLambda = true
      case _         =>
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
