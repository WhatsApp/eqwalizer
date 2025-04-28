/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.PipelineContext

object ElimTypeVars {
  sealed trait VarElimMode
  case object Promote extends VarElimMode
  case object Demote extends VarElimMode

  private def containsVars(ty: Type, tv: Set[Int]): Boolean = ty match {
    case VarType(n) => tv(n)
    case ty         => TypeVars.children(ty).exists(containsVars(_, tv))
  }

  /** Pierce and Turner Local Type Inference section 3.2
    */
  def elimTypeVars(ty: Type, mode: VarElimMode, vars: Set[Int])(implicit pipelineContext: PipelineContext): Type = {
    def elim(t: Type): Type = elimTypeVars(t, mode, vars)
    ty match {
      case FunType(forall, args, resType) =>
        val args1 = args.map(elimTypeVars(_, switchMode(mode), vars))
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
        val variances = pipelineContext.variance.paramVariances(id)
        val elimmedParams = params.lazyZip(variances).map {
          case (param, Variance.Constant | Variance.Covariant) => elimTypeVars(param, mode, vars)
          case (param, Variance.Contravariant)                 => elimTypeVars(param, switchMode(mode), vars)
          case (param, Variance.Invariant) =>
            if (containsVars(param, vars)) modeToType(mode)
            else param
        }
        RemoteType(id, elimmedParams)
      case VarType(v) if vars.contains(v) =>
        modeToType(mode)
      case vt: VarType =>
        vt
      case MapType(props, kt, vt) =>
        MapType(props.map { case (key, Prop(req, tp)) => (key, Prop(req, elim(tp))) }, elim(kt), elim(vt))
      case BoundedDynamicType(bound) =>
        BoundedDynamicType(elim(bound))
      case _ =>
        ty
    }
  }

  private def modeToType(mode: VarElimMode): Type = mode match {
    case Promote => AnyType
    case Demote  => NoneType
  }

  private def switchMode(mode: VarElimMode): VarElimMode = mode match {
    case Promote => Demote
    case Demote  => Promote
  }
}
