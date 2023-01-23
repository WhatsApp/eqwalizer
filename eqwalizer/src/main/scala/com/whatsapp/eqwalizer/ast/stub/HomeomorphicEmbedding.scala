/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Types._

// $COVERAGE-OFF$
private object HomeomorphicEmbedding {
  private def isHe(s: Type, t: Type): Boolean = {
    heByDiving(s, t) || heByCoupling(s, t)
  }

  private def heByDiving(s: Type, t: Type): Boolean =
    t match {
      case FunType(forall, argTys, resTy) =>
        assert(forall.isEmpty, s"function types in eqWAlizer type aliases are not expected to have nonempty foralls")
        (resTy :: argTys).exists(isHe(s, _))
      case AnyArityFunType(resTy) =>
        isHe(s, resTy)
      case TupleType(argTys) =>
        argTys.exists(isHe(s, _))
      case ListType(t2) =>
        isHe(s, t2)
      case UnionType(tys) =>
        tys.exists(isHe(s, _))
      case RemoteType(id, argTys) =>
        argTys.exists(isHe(s, _))
      case OpaqueType(id, argTys) =>
        argTys.exists(isHe(s, _))
      case DictMap(kt, vt) =>
        isHe(s, kt) || isHe(s, vt)
      case ShapeMap(props) =>
        props.map(_.tp).exists(isHe(s, _))
      case RefinedRecordType(_, fields) =>
        fields.values.exists(isHe(s, _))
      case _ =>
        false
    }

  def heByCoupling(s: Type, t: Type): Boolean =
    (s, t) match {
      case (TupleType(argTys1), TupleType(argTys2)) if argTys1.size == argTys2.size =>
        argTys1.lazyZip(argTys2).forall(isHe)
      case (_: TupleType, _) =>
        false
      case (FunType(forall1, argTys1, resTy1), FunType(forall2, argTys2, resTy2)) if argTys1.size == argTys2.size =>
        assert(forall1.isEmpty, s"function types in eqWAlizer type aliases are not expected to have nonempty foralls")
        assert(forall2.isEmpty, s"function types in eqWAlizer type aliases are not expected to have nonempty foralls")
        (resTy1 :: argTys1).lazyZip(resTy2 :: argTys2).forall(isHe)
      case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
        isHe(resTy1, resTy2)
      case (_: FunType, _) =>
        false
      case (ListType(elemTy1), ListType(elemTy2)) =>
        isHe(elemTy1, elemTy2)
      case (_: ListType, _) =>
        false
      case (UnionType(tys1), UnionType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(isHe)
      case (_: UnionType, _) =>
        false
      case (RemoteType(id1, argTys1), RemoteType(id2, argTys2)) if id1 == id2 =>
        argTys1.lazyZip(argTys2).forall(isHe)
      case (_: RemoteType, _) =>
        false
      case (OpaqueType(id1, argTys1), OpaqueType(id2, argTys2)) if id1 == id2 =>
        argTys1.lazyZip(argTys2).forall(isHe)
      case (_: OpaqueType, _) =>
        false
      case (DictMap(kType1, vType1), DictMap(kType2, vType2)) =>
        isHe(kType1, kType2) && isHe(vType1, vType2)
      case (_: DictMap, _) =>
        false
      case (ShapeMap(props1), ShapeMap(props2)) if props1.size == props2.size =>
        props1.sortBy(_.key).lazyZip(props2.sortBy(_.key)).forall(heProp)
      case (RefinedRecordType(recTy1, fields1), RefinedRecordType(recTy2, fields2)) if recTy1 == recTy2 =>
        fields1.forall(f => fields2.contains(f._1)) &&
        fields2.forall(f => fields1.contains(f._1)) &&
        fields1.forall { case (fieldName, fieldTy) => isHe(fieldTy, fields2(fieldName)) }
      case (_: RefinedRecordType, _) =>
        false
      case (_: ShapeMap, _) =>
        false
      case (VarType(sVar), VarType(tVar)) if sVar == tVar =>
        true
      case (_: VarType, _) =>
        false
      case _ =>
        s == t
    }

  private def heProp(sProp: Prop, tProp: Prop): Boolean = (sProp, tProp) match {
    case (ReqProp(sKey, sTy), ReqProp(tKey, tTy)) if sKey == tKey =>
      isHe(sTy, tTy)
    case (_: ReqProp, _) =>
      false
    case (OptProp(sKey, sTy), OptProp(tKey, tTy)) if sKey == tKey =>
      isHe(sTy, tTy)
    case (_: OptProp, _) =>
      false
  }
}
