/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types.*

object TypeVars {

  class Instantiate {
    private var shift = 0

    def instantiate(ft: FunType): (List[Int], FunType) =
      if (ft.forall == 0) (Nil, ft)
      else {
        val newVars = (shift until (shift + ft.forall)).toList
        val res = FunType(0, ft.argTys.map(substLevels(shift)), substLevels(shift)(ft.resTy))
        shift = shift + ft.forall
        (newVars, res)
      }
  }

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

  def hasTypeVars(ty: Type): Boolean = ty match {
    case FreeVarType(_) => true
    case ty             => children(ty).exists(hasTypeVars)
  }

  /** note: returns Nil for record types because they can't have type vars
    */
  def children(ty: Type): List[Type] = ty match {
    case FunType(_, argTys, resTy)    => resTy :: argTys
    case AnyArityFunType(resTy)       => resTy :: Nil
    case TupleType(argTys)            => argTys
    case UnionType(tys)               => tys.toList
    case RemoteType(_, tys)           => tys
    case MapType(props, kType, vType) => kType :: vType :: props.values.map(_.tp).toList
    case ListType(ty)                 => ty :: Nil
    case RefinedRecordType(_, fields) => fields.toList.map(_._2)
    case BoundedDynamicType(bound)    => bound :: Nil
    case _                            => Nil
  }

  def conformForalls(ft1: FunType, ft2: FunType): Option[(FunType, FunType)] =
    if (ft1.forall != ft2.forall || ft1.argTys.size != ft2.argTys.size) None
    else Some(ft1, ft2)

  private def substLevels(shift: Int)(t: Type): Type = t match {
    case bv: BoundVarType =>
      FreeVarType(bv.lvl + shift)(bv.name)
    case FunType(forall, args, resType) =>
      assert(forall == 0, "There should be no nested forall")
      FunType(forall, args.map(substLevels(shift)), substLevels(shift)(resType))
    case AnyArityFunType(resType) =>
      AnyArityFunType(substLevels(shift)(resType))
    case TupleType(params) =>
      TupleType(params.map(substLevels(shift)))
    case ListType(elemT) =>
      ListType(substLevels(shift)(elemT))
    case UnionType(params) =>
      UnionType(params.map(substLevels(shift)))
    case RemoteType(id, params) =>
      RemoteType(id, params.map(substLevels(shift)))
    case MapType(props, kt, vt) =>
      MapType(
        props.map { case (key, Prop(req, tp)) => (key, Prop(req, substLevels(shift)(tp))) },
        substLevels(shift)(kt),
        substLevels(shift)(vt),
      )
    case RefinedRecordType(recType, fields) =>
      RefinedRecordType(recType, fields.map(f => f._1 -> substLevels(shift)(f._2)))
    case BoundedDynamicType(bound) =>
      BoundedDynamicType(substLevels(shift)(bound))
    case _ =>
      t
  }

  private def containsVars(ty: Type, tv: Set[Int]): Boolean = ty match {
    case FreeVarType(n) => tv(n)
    case ty             => TypeVars.children(ty).exists(containsVars(_, tv))
  }

  def promote(ty: Type, vars: Set[Int]): Type =
    elimTypeVars(ty, ElimMode.Promote, vars)

  def demote(ty: Type, vars: Set[Int]): Type =
    elimTypeVars(ty, ElimMode.Demote, vars)

  private def elimTypeVars(ty: Type, mode: ElimMode, vars: Set[Int]): Type = {
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
