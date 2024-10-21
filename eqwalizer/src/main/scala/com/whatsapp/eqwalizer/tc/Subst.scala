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
      case AnyArityFunType(resTy) =>
        AnyArityFunType(sub(resTy))
      case TupleType(params) =>
        TupleType(params.map(sub))
      case ListType(elemT) =>
        ListType(sub(elemT))
      case UnionType(params) =>
        UnionType(params.map(sub))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(sub))
      case OpaqueType(id, params) =>
        OpaqueType(id, params.map(sub))
      case VarType(n) =>
        s.getOrElse(n, t)
      case MapType(props, kTy, vTy) =>
        MapType(props.map { case (key, prop) => (key, Prop(prop.req, sub(prop.tp))) }, sub(kTy), sub(vTy))
      case RefinedRecordType(recType, fields) => RefinedRecordType(recType, fields.map(f => f._1 -> sub(f._2)))
      case BoundedDynamicType(bound)          => BoundedDynamicType(sub(bound))
      case _ =>
        t
    }
  }
}
