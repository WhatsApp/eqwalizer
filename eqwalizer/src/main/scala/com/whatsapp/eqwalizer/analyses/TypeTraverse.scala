/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Types._

class TypeTraverse(val listener: TypeListener) {
  def traverse(tp: Type): Unit = tp match {
    // $COVERAGE-OFF$
    case at @ AtomLitType(_) =>
      listener.enterType(at)
      listener.exitType(at)
    case aft @ AnyFunType =>
      listener.enterType(aft)
      listener.exitType(aft)
    case ft @ FunType(forall, argTys, resTy) =>
      listener.enterType(ft)
      argTys.foreach(traverse)
      traverse(resTy)
      listener.exitType(ft)
    case att @ AnyTupleType =>
      listener.enterType(att)
      listener.exitType(att)
    case tt @ TupleType(elemTys) =>
      listener.enterType(tt)
      elemTys.foreach(traverse)
      listener.exitType(tt)
    case nt @ NilType =>
      listener.enterType(nt)
      listener.exitType(nt)
    case lt @ ListType(elemTy) =>
      listener.enterType(lt)
      traverse(elemTy)
      listener.exitType(lt)
    case ut @ UnionType(tys) =>
      listener.enterType(ut)
      tys.foreach(traverse)
      listener.exitType(ut)
    case rt @ RemoteType(id, argTys) =>
      listener.enterType(rt)
      argTys.foreach(traverse)
      listener.exitType(rt)
    case ot @ OpaqueType(id, argTys) =>
      listener.enterType(ot)
      argTys.foreach(traverse)
      listener.exitType(ot)
    case vt @ VarType(n) =>
      listener.enterType(vt)
      listener.exitType(vt)
    case rt @ RecordType(name) =>
      listener.enterType(rt)
      listener.exitType(rt)
    case rt @ RefinedRecordType(recTy, fields) =>
      listener.enterType(rt)
      fields.foreach(f => traverse(f._2))
      listener.exitType(rt)
    case dt @ DictMap(kType, vType) =>
      listener.enterType(dt)
      traverse(kType)
      traverse(vType)
      listener.exitType(dt)
    case st @ ShapeMap(props) =>
      listener.enterType(st)
      props.foreach(p => traverse(p.tp))
      listener.exitType(st)
    case bt @ BinaryType =>
      listener.enterType(bt)
      listener.exitType(bt)
    case bt: BuiltinType =>
      listener.enterType(bt)
      listener.exitType(bt)
  }
}
