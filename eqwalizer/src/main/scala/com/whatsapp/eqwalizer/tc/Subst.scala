/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types._

object Subst {
  def subst(s: Map[Int, Type], t: Type): Type = {
    def sub(ty: Type): Type = subst(s, ty)
    t match {
      case FunType(forall, args, resType) =>
        val s1 = s -- forall
        FunType(forall, args.map(subst(s1, _)), subst(s1, resType))
      case TupleType(params) =>
        TupleType(params.map(sub))
      case ListType(elemT) =>
        ListType(sub(elemT))
      case UnionType(params) =>
        UnionType(params.map(sub))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(sub))
      case OpaqueType(id, params) =>
        // $COVERAGE-OFF$
        OpaqueType(id, params.map(sub))
      // $COVERAGE-ON$
      case VarType(n) =>
        s.getOrElse(n, t)
      case ShapeMap(props)                    => ShapeMap(props.map(substInProp(s, _)))
      case DictMap(kTy, vTy)                  => DictMap(sub(kTy), sub(vTy))
      case RefinedRecordType(recType, fields) => RefinedRecordType(recType, fields.map(f => f._1 -> sub(f._2)))
      case _ =>
        t
    }
  }

  private def substInProp(s: Map[Int, Type], prop: Prop): Prop =
    prop match {
      case ReqProp(key, tp) =>
        ReqProp(key, subst(s, tp))
      case OptProp(key, tp) =>
        OptProp(key, subst(s, tp))
    }
}
