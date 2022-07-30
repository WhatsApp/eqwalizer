/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.Types._

object ElimTypeVars {
  sealed trait VarElimMode
  case object Promote extends VarElimMode
  case object Demote extends VarElimMode

  /** Pierce and Turner Local Type Inference section 3.2
    */
  def elimTypeVars(ty: Type, mode: VarElimMode, vars: Set[Int]): Type = {
    def elim(t: Type): Type = elimTypeVars(t, mode, vars)
    ty match {
      case FunType(forall, args, resType) =>
        val args1 = args.map(elimTypeVars(_, switchMode(mode), vars))
        FunType(forall, args1, elim(resType))
      case TupleType(params) =>
        TupleType(params.map(elim))
      case ListType(elemT) =>
        ListType(elim(elemT))
      case UnionType(params) =>
        UnionType(params.map(elim))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(elim))
      case OpaqueType(id, params) =>
        // $COVERAGE-OFF$
        OpaqueType(id, params.map(elim))
      // $COVERAGE-ON$
      case VarType(v) if vars.contains(v) =>
        modeToType(mode)
      case vt: VarType =>
        vt
      case DictMap(kt, vt) =>
        DictMap(elim(kt), elim(vt))
      case ShapeMap(props) =>
        ShapeMap(props.map(elimVarsInProp(_, mode, vars)))
      case _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RefinedRecordType | _: RecordType |
          AnyTupleType | AnyFunType =>
        ty
    }
  }

  private def elimVarsInProp(prop: Prop, mode: VarElimMode, vars: Set[Int]): Prop = prop match {
    case ReqProp(key, ty) => ReqProp(key, elimTypeVars(ty, mode, vars))
    case OptProp(key, ty) => OptProp(key, elimTypeVars(ty, mode, vars))
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
