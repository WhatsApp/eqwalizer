/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.ExternalTypes._

object ExternalTypeVars {
  def children(ty: ExtType): List[ExtType] = ty match {
    case FunExtType(argTys, resTy) =>
      resTy :: argTys
    case AnyArityFunExtType(resTy) =>
      resTy :: Nil
    case TupleExtType(argTys) =>
      argTys
    case UnionExtType(tys) =>
      tys
    case MapExtType(props) =>
      props.flatMap(p => List(p.key, p.tp))
    case ListExtType(ty) =>
      ty :: Nil
    case RecordRefinedExtType(_, fields) =>
      fields.flatMap(f => children(f.ty))
    case RemoteExtType(_, args) =>
      args
    case _: AtomLitExtType | _: VarExtType | _: RecordExtType | _: AnyMapExtType | _: LocalExtType | _: BuiltinExtType |
        _: IntLitExtType | _: UnOpType | _: BinOpType | _: AnyListExtType =>
      Nil
  }

  def findTypeVarInUnion(ty: ExtType)(implicit isInUnionType: Boolean = false): Option[VarExtType] = ty match {
    case vt: VarExtType if isInUnionType => Some(vt)
    case _: UnionExtType =>
      ExternalTypeVars.children(ty).flatMap(findTypeVarInUnion(_)(isInUnionType = true)).headOption
    case _ => ExternalTypeVars.children(ty).flatMap(findTypeVarInUnion).headOption
  }

  def collectTyVars(extTy: ExtType): List[VarExtType] = extTy match {
    case vt: VarExtType => vt :: Nil
    case _              => ExternalTypeVars.children(extTy).flatMap(collectTyVars)
  }
}
