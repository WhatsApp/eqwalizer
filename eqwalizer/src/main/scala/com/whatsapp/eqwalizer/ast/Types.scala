/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms.RecDeclTyped

object Types {
  sealed trait Type
  case class AtomLitType(atom: String) extends Type
  case object AnyFunType extends Type
  case class FunType(forall: List[Int], argTys: List[Type], resTy: Type) extends Type
  case class AnyArityFunType(resTy: Type) extends Type

  case object AnyTupleType extends Type
  case class TupleType(argTys: List[Type]) extends Type
  case object NilType extends Type
  case class ListType(t: Type) extends Type

  /** prefer `Subtype.join` over `UnionType.apply`
    */
  case class UnionType(tys: Set[Type]) extends Type
  case class RemoteType(id: RemoteId, argTys: List[Type]) extends Type
  case class OpaqueType(id: RemoteId, argTys: List[Type]) extends Type

  case class VarType(n: Int)(val name: String) extends Type
  case class RecordType(name: String)(val module: String) extends Type
  case class RefinedRecordType(recType: RecordType, fields: Map[String, Type]) extends Type
  case class DictMap(kType: Type, vType: Type) extends Type
  case class ShapeMap(props: List[Prop]) extends Type
  case object BinaryType extends Type

  sealed trait Prop {
    val key: String
    val tp: Type
  }
  case class ReqProp(key: String, tp: Type) extends Prop
  case class OptProp(key: String, tp: Type) extends Prop

  case object AnyType extends Type
  case object AtomType extends Type
  case object DynamicType extends Type
  case object NoneType extends Type
  case object PidType extends Type
  case object PortType extends Type
  case object ReferenceType extends Type
  case object NumberType extends Type

  private val ioListRid = RemoteId("erlang", "iolist", 0)

  val falseType: Type = AtomLitType("false")
  val trueType: Type = AtomLitType("true")
  val charType: Type = NumberType
  val byteType: Type = NumberType
  val floatType: Type = NumberType
  val undefined: Type = AtomLitType("undefined")
  val exnClassType: Type =
    UnionType(Set(AtomLitType("error"), AtomLitType("exit"), AtomLitType("throw")))
  val clsExnStackType: Type =
    TupleType(List(exnClassType, AnyType, ListType(AnyType)))
  val clsExnStackTypeDynamic: Type =
    TupleType(List(exnClassType, DynamicType, ListType(DynamicType)))

  val builtinTypeAliasBodies = Map(
    "string" -> ListType(charType),
    "boolean" -> UnionType(Set(falseType, trueType)),
    "timeout" -> UnionType(Set(AtomLitType("infinity"), NumberType)),
    "identifier" -> UnionType(Set(PidType, PortType, ReferenceType)),
    "mfa" -> TupleType(List(AtomType, AtomType, NumberType)),
    "iolist" -> ListType(UnionType(Set(byteType, BinaryType, RemoteType(ioListRid, Nil)))),
    "iodata" -> UnionType(Set(RemoteType(RemoteId("erlang", "iolist", 0), Nil), BinaryType)),
  )

  val builtinTypeAliases =
    builtinTypeAliasBodies.keySet.map(name => name -> RemoteType(RemoteId("erlang", name, 0), Nil)).toMap

  val stringType = builtinTypeAliases("string")
  val booleanType = builtinTypeAliases("boolean")

  def join(tys: Set[Type]): Type = {
    def collect(ty: Type): Set[Type] = {
      ty match {
        case UnionType(tys) => tys.flatMap(collect)
        case _              => Set(ty)
      }
    }
    val allTys = tys.flatMap(collect)
    if (allTys.contains(AnyType)) AnyType
    else UnionType(allTys)
  }

  def recordAsTuple(recDecl: RecDeclTyped): TupleType = {
    val elems = AtomLitType(recDecl.name) :: recDecl.fields.values.toList.map(_.tp)
    TupleType(elems)
  }

  def refinedRecordAsTuple(recDecl: RecDeclTyped, refinedRecord: RefinedRecordType): TupleType = {
    val fieldsList = recDecl.fields.map(f => refinedRecord.fields.getOrElse(f._1, f._2.tp)).toList
    TupleType(AtomLitType(refinedRecord.recType.name) :: fieldsList)
  }

  val builtinTypes: Map[String, Type] =
    Map(
      "any" -> AnyType,
      "atom" -> AtomType,
      "binary" -> BinaryType,
      "bitstring" -> BinaryType,
      "byte" -> byteType,
      "char" -> charType,
      "float" -> floatType,
      "fun" -> AnyFunType,
      "function" -> AnyFunType,
      "maybe_improper_list" -> ListType(AnyType),
      "nonempty_maybe_improper_list" -> ListType(AnyType),
      "pos_integer" -> NumberType,
      "neg_integer" -> NumberType,
      "non_neg_integer" -> NumberType,
      "integer" -> NumberType,
      "nil" -> NilType,
      "none" -> NoneType,
      "number" -> NumberType,
      "pid" -> PidType,
      "port" -> PortType,
      "reference" -> ReferenceType,
      "term" -> AnyType,
      "tuple" -> AnyTupleType,
      "arity" -> NumberType,
      "module" -> AtomType,
      "node" -> AtomType,
      "no_return" -> NoneType,
      "nonempty_string" -> stringType,
    ) ++ builtinTypeAliases
}
