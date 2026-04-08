/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.stub.Db

enum Variance {
  case Constant, Covariant, Contravariant, Invariant
}

object Variance {
  private type Var = Int

  def toVariances(ft: FunType, vars: List[Var]): Map[Var, Variance] =
    vars.map(tv => tv -> toTopLevelVariance(ft, tv)).toMap

  def paramVariances(remoteId: RemoteId): List[Variance] =
    Db.getVariance(remoteId.module, Id(remoteId.name, remoteId.arity)).get

  private def varianceOf(ty: Type, tv: Var, isPositivePosition: Boolean): Variance = ty match {
    case FreeVarType(n) if tv == n =>
      if (isPositivePosition) Covariant
      else Contravariant
    case FunType(forall, argTys, resTy) =>
      val variancesInArgTys = argTys.map(varianceOf(_, tv, !isPositivePosition))
      val variances = varianceOf(resTy, tv, isPositivePosition) :: variancesInArgTys
      combineVariances(variances)
    case RemoteType(rid, args) =>
      val pvs = paramVariances(rid)
      combineVariances(args.zip(pvs).map { case (arg, pv) =>
        pv match {
          case Covariant     => varianceOf(arg, tv, isPositivePosition)
          case Contravariant => varianceOf(arg, tv, !isPositivePosition)
          case Invariant     => if (containsVar(arg, tv)) Invariant else Constant
          case Constant      => Constant
        }
      })
    case _ =>
      val variances = TypeVars.children(ty).map(varianceOf(_, tv, isPositivePosition))
      combineVariances(variances)
  }

  private def containsVar(ty: Type, tv: Var): Boolean = ty match {
    case FreeVarType(n) => tv == n
    case _              => TypeVars.children(ty).exists(containsVar(_, tv))
  }

  private def toTopLevelVariance(ft: FunType, tv: Var): Variance =
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

  private def combineVariances(variances: List[Variance]): Variance =
    variances.foldLeft(Constant: Variance)((v1Opt, v2Opt) =>
      (v1Opt, v2Opt) match {
        case (v, Constant)        => v
        case (Constant, v)        => v
        case (v1, v2) if v1 == v2 => v1
        case (_, _)               => Invariant
      }
    )
}
