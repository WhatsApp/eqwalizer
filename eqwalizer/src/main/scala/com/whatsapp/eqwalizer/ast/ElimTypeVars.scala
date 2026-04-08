/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types.*

enum ElimMode {
  case Promote, Demote

  def switch: ElimMode = this match {
    case Promote => Demote
    case Demote  => Promote
  }

  def toType: Type = this match {
    case Promote => AnyType
    case Demote  => NoneType
  }
}

object ElimTypeVars {
  private def containsVars(ty: Type, tv: Set[Int]): Boolean = ty match {
    case FreeVarType(n) => tv(n)
    case ty             => TypeVars.children(ty).exists(containsVars(_, tv))
  }

  def elimTypeVars(ty: Type, mode: ElimMode, vars: Set[Int]): Type = {
    def elim(t: Type): Type = elimTypeVars(t, mode, vars)
    ty match {
      case FunType(forall, args, resType) =>
        val args1 = args.map(elimTypeVars(_, mode.switch, vars))
        FunType(forall, args1, elim(resType))
      case AnyArityFunType(resType) =>
        AnyArityFunType(elim(resType))
      case TupleType(params) =>
        TupleType(params.map(elim))
      case ListType(elemT) =>
        ListType(elim(elemT))
      case UnionType(params) =>
        UnionType(params.map(elim))
      case RemoteType(id, params) =>
        val variances = Variance.paramVariances(id)
        val elimmedParams = params.lazyZip(variances).map {
          case (param, Variance.Constant | Variance.Covariant) => elimTypeVars(param, mode, vars)
          case (param, Variance.Contravariant)                 => elimTypeVars(param, mode.switch, vars)
          case (param, Variance.Invariant) =>
            if (containsVars(param, vars)) mode.toType
            else param
        }
        RemoteType(id, elimmedParams)
      case FreeVarType(v) if vars.contains(v) =>
        mode.toType
      case vt: FreeVarType =>
        vt
      case MapType(props, kt, vt) =>
        MapType(props.map { case (key, Prop(req, tp)) => (key, Prop(req, elim(tp))) }, elim(kt), elim(vt))
      case BoundedDynamicType(bound) =>
        BoundedDynamicType(elim(bound))
      case _ =>
        ty
    }
  }
}
