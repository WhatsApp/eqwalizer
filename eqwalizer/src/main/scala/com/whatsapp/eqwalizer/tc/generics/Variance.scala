/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types._

object Variance {
  private type Var = Int

  sealed trait Variance
  object ConstantOrCovariant extends Variance
  object Contravariant extends Variance
  object Invariant extends Variance

  def toVariances(ft: FunType): Map[Var, Variance] =
    ft.forall.map(tv => tv -> toTopLevelVariance(ft, tv)).toMap

  def varianceOf(ty: Type, tv: Var, isPositivePosition: Boolean): Option[Variance] = ty match {
    case VarType(n) if tv == n =>
      if (isPositivePosition) Some(ConstantOrCovariant)
      else Some(Contravariant)
    case FunType(forall, argTys, resTy) =>
      val variancesInArgTys = if (forall.contains(tv)) {
        // $COVERAGE-OFF$
        Nil
        // $COVERAGE-ON$
      } else {
        argTys.map(varianceOf(_, tv, !isPositivePosition))
      }
      val variances = varianceOf(resTy, tv, isPositivePosition) :: variancesInArgTys
      combineVariances(variances)
    case _ =>
      val variances = TypeVars.children(ty).map(varianceOf(_, tv, isPositivePosition))
      combineVariances(variances)
  }

  private def toTopLevelVariance(ft: FunType, tv: Var): Variance =
    varianceOf(ft.resTy, tv, isPositivePosition = true) match {
      case Some(variance) =>
        variance
      case None =>
        combineVariances(ft.argTys.map(varianceOf(_, tv, isPositivePosition = false))).get match {
          case ConstantOrCovariant | Invariant =>
            ConstantOrCovariant
          case Contravariant =>
            Contravariant
        }
    }

  private def combineVariances(variances: List[Option[Variance]]): Option[Variance] =
    variances.foldLeft(None: Option[Variance])((v1Opt, v2Opt) =>
      (v1Opt, v2Opt) match {
        case (None, Some(v))                  => Some(v)
        case (Some(v), None)                  => Some(v)
        case (None, None)                     => None
        case (Some(v1), Some(v2)) if v1 == v2 => Some(v1)
        case (Some(_), Some(_))               => Some(Invariant)
      }
    )
}
