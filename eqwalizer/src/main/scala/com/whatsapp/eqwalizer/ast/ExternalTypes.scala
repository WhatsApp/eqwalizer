/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

object ExternalTypes {
  sealed trait ExtType {
    val pos: Pos
  }
  case class AtomLitExtType(atom: String)(val pos: Pos) extends ExtType
  case class FunExtType(argTys: List[ExtType], resTy: ExtType)(val pos: Pos) extends ExtType
  case class AnyArityFunExtType(resTy: ExtType)(val pos: Pos) extends ExtType

  case class TupleExtType(argTys: List[ExtType])(val pos: Pos) extends ExtType
  case class ListExtType(t: ExtType)(val pos: Pos) extends ExtType
  case class AnyListExtType()(val pos: Pos) extends ExtType

  case class UnionExtType(tys: List[ExtType])(val pos: Pos) extends ExtType
  case class LocalExtType(id: Id, args: List[ExtType])(val pos: Pos) extends ExtType
  case class RemoteExtType(id: RemoteId, args: List[ExtType])(val pos: Pos) extends ExtType
  case class BuiltinExtType(name: String)(val pos: Pos) extends ExtType
  case class IntLitExtType()(val pos: Pos) extends ExtType
  case class UnOpType(op: String)(val pos: Pos) extends ExtType
  case class BinOpType(op: String)(val pos: Pos) extends ExtType

  case class VarExtType(name: String)(val pos: Pos) extends ExtType
  case class RecordExtType(name: String)(val pos: Pos) extends ExtType
  case class RecordRefinedExtType(name: String, refinedFields: List[RefinedField])(val pos: Pos) extends ExtType
  case class MapExtType(props: List[ExtProp])(val pos: Pos) extends ExtType
  case class AnyMapExtType()(val pos: Pos) extends ExtType

  case class ConstrainedFunType(ty: FunExtType, constraints: List[Constraint])(val pos: Pos)
  case class Constraint(tVar: String, ty: ExtType)(val pos: Pos)
  case class RefinedField(label: String, ty: ExtType)

  sealed trait ExtProp {
    val key: ExtType
    val tp: ExtType
    val pos: Pos
  }
  case class ReqExtProp(key: ExtType, tp: ExtType)(val pos: Pos) extends ExtProp
  case class ReqBadExtProp(key: ExtType, tp: ExtType)(val pos: Pos) extends ExtProp
  case class OptExtProp(key: ExtType, tp: ExtType)(val pos: Pos) extends ExtProp
  case class OptBadExtProp(key: ExtType, tp: ExtType)(val pos: Pos) extends ExtProp

  def anyExtType(pos: Pos): ExtType = BuiltinExtType("any")(pos)
  def intExtType(pos: Pos): ExtType = BuiltinExtType("integer")(pos)

  def isAnyExtType(ty: ExtType): Boolean = ty match {
    case BuiltinExtType("term") => true
    case BuiltinExtType("any")  => true
    case _                      => false
  }

}
