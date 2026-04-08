/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types.*

object Subst {
  def subst(s: Map[Int, Type], t: Type): Type = {
    def sub(ty: Type): Type = subst(s, ty)
    t match {
      case MapType(props, kTy, vTy) =>
        MapType(props.map { case (key, prop) => (key, Prop(prop.req, sub(prop.tp))) }, sub(kTy), sub(vTy))
      case FunType(n, args, resType)          => FunType(n, args.map(subst(s, _)), subst(s, resType))
      case AnyArityFunType(resTy)             => AnyArityFunType(sub(resTy))
      case TupleType(params)                  => TupleType(params.map(sub))
      case ListType(elemT)                    => ListType(sub(elemT))
      case UnionType(params)                  => UnionType(params.map(sub))
      case RemoteType(id, params)             => RemoteType(id, params.map(sub))
      case FreeVarType(n)                     => s.getOrElse(n, t)
      case RefinedRecordType(recType, fields) => RefinedRecordType(recType, fields.map(f => f._1 -> sub(f._2)))
      case BoundedDynamicType(bound)          => BoundedDynamicType(sub(bound))
      case _                                  => t
    }
  }
}
