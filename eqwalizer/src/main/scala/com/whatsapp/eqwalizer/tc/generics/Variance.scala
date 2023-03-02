/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.{Id, RemoteId, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.PipelineContext

class Variance(pipelineContext: PipelineContext) {
  private type Var = Int

  import Variance._

  val util = pipelineContext.util

  def toVariances(ft: FunType): Map[Var, Variance.Variance] =
    ft.forall.map(tv => tv -> toTopLevelVariance(ft, tv)).toMap

  def paramVariances(remoteId: RemoteId): List[Variance.Variance] = {
    val id = Id(remoteId.name, remoteId.arity)
    DbApi.getType(remoteId.module, id) match {
      case Some(tDecl) =>
        tDecl.params.map(varType => varianceOf(tDecl.body, varType.n, isPositivePosition = true).get)
      case None =>
        // Opaques are covariant
        DbApi.getPrivateOpaque(remoteId.module, id).get.params.map(_ => Covariant)
    }
  }

  def varianceOf(ty: Type, tv: Var, isPositivePosition: Boolean): Option[Variance.Variance] =
    getVarianceOf(ty, tv, isPositivePosition)(history = Set())

  private def getVarianceOf(ty: Type, tv: Var, isPositivePosition: Boolean)(implicit
      history: Set[(RemoteType, Boolean)]
  ): Option[Variance.Variance] = ty match {
    case VarType(n) if tv == n =>
      if (isPositivePosition) Some(Covariant)
      else Some(Contravariant)
    case FunType(forall, argTys, resTy) =>
      val variancesInArgTys = if (forall.contains(tv)) {
        Nil
      } else {
        argTys.map(getVarianceOf(_, tv, !isPositivePosition))
      }
      val variances = getVarianceOf(resTy, tv, isPositivePosition) :: variancesInArgTys
      combineVariances(variances)
    case t @ RemoteType(rid, args) =>
      if (history((t, isPositivePosition))) {
        Some(Constant)
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
      case Some(variance) =>
        variance
      case None =>
        combineVariances(ft.argTys.map(varianceOf(_, tv, isPositivePosition = false))).getOrElse(Constant) match {
          case Constant | Covariant | Invariant =>
            Covariant
          case Contravariant =>
            Contravariant
        }
    }

  def combineVariances(variances: List[Option[Variance.Variance]]): Option[Variance.Variance] =
    variances.foldLeft(None: Option[Variance.Variance])((v1Opt, v2Opt) =>
      (v1Opt, v2Opt) match {
        case (None, Some(v))                  => Some(v)
        case (Some(v), None)                  => Some(v)
        case (v, Some(Constant))              => v
        case (Some(Constant), v)              => v
        case (None, None)                     => None
        case (Some(v1), Some(v2)) if v1 == v2 => Some(v1)
        case (Some(_), Some(_))               => Some(Invariant)
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
