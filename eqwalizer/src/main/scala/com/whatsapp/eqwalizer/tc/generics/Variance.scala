/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.{Id, RemoteId, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc.PipelineContext

class Variance(pipelineContext: PipelineContext) {
  private type Var = Int

  import Variance._

  val util = pipelineContext.util

  def toVariances(ft: FunType): Map[Var, Variance.Variance] =
    ft.forall.map(tv => tv -> toTopLevelVariance(ft, tv)).toMap

  def paramVariances(remoteId: RemoteId): List[Variance.Variance] = {
    val id = Id(remoteId.name, remoteId.arity)
    val tDecl = Db.getType(remoteId.module, id).get
    tDecl.params.map(varType => varianceOf(tDecl.body, varType.n, isPositivePosition = true))
  }

  private def varianceOf(ty: Type, tv: Var, isPositivePosition: Boolean): Variance.Variance =
    getVarianceOf(ty, tv, isPositivePosition)(history = Set())

  private def getVarianceOf(ty: Type, tv: Var, isPositivePosition: Boolean)(implicit
      history: Set[(RemoteType, Boolean)]
  ): Variance.Variance = ty match {
    case VarType(n) if tv == n =>
      if (isPositivePosition) Covariant
      else Contravariant
    case FunType(forall, argTys, resTy) =>
      // forall can only be non-empty only for top-level fun types
      // corresponding to generic specs
      val variancesInArgTys = argTys.map(getVarianceOf(_, tv, !isPositivePosition))
      val variances = getVarianceOf(resTy, tv, isPositivePosition) :: variancesInArgTys
      combineVariances(variances)
    case t @ RemoteType(rid, args) =>
      if (history((t, isPositivePosition))) {
        Constant
      } else {
        val body = util.getTypeDeclBody(rid, args)
        getVarianceOf(body, tv, isPositivePosition)(history + ((t, isPositivePosition)))
      }
    case _ =>
      val variances = TypeVars.children(ty).map(getVarianceOf(_, tv, isPositivePosition))
      combineVariances(variances)
  }

  private def toTopLevelVariance(ft: FunType, tv: Var): Variance.Variance =
    varianceOf(ft.resTy, tv, isPositivePosition = true) match {
      case Constant =>
        combineVariances(ft.argTys.map(varianceOf(_, tv, isPositivePosition = false))) match {
          case Constant | Covariant | Invariant =>
            Covariant
          case Contravariant =>
            Contravariant
        }
      case variance =>
        variance
    }

  private def combineVariances(variances: List[Variance.Variance]): Variance.Variance =
    variances.foldLeft(Constant: Variance.Variance)((v1Opt, v2Opt) =>
      (v1Opt, v2Opt) match {
        case (v, Constant)        => v
        case (Constant, v)        => v
        case (v1, v2) if v1 == v2 => v1
        case (_, _)               => Invariant
      }
    )
}

object Variance {

  sealed trait Variance
  object Constant extends Variance
  object Covariant extends Variance
  object Contravariant extends Variance
  object Invariant extends Variance

}
