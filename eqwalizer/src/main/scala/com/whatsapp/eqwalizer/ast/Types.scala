/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Expr, Tuple}
import com.whatsapp.eqwalizer.ast.Forms.RecDecl
import com.whatsapp.eqwalizer.ast.Guards.{Test, TestAtom, TestTuple}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.util.boundary

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
  case class MapType(props: Map[Key, Prop], kType: Type, vType: Type) extends Type

  object MapType {
    def apply(props: Map[Key, Prop], kType: Type, vType: Type) = {
      Key.fromType(kType) match {
        case Some(key) => new MapType(props + (key -> Prop(req = false, vType)), NoneType, NoneType)
        case None =>
          if (kType == NoneType) new MapType(props, NoneType, NoneType)
          else new MapType(props, kType, vType)
      }
    }
  }

  sealed trait Key
  case class TupleKey(keys: List[Key]) extends Key {
    override def toString: String = keys.map(_.toString).mkString("{", ", ", "}")
  }
  case class AtomKey(name: String) extends Key {
    override def toString: String = name
  }

  case class Prop(req: Boolean, tp: Type)

  case object BinaryType extends Type
  case object AnyType extends Type
  case object AtomType extends Type
  case object DynamicType extends Type
  case class BoundedDynamicType(bound: Type) extends Type
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
  val clsExnStackTypeDynamic: Type =
    TupleType(List(exnClassType, DynamicType, ListType(DynamicType)))

  val builtinTypeAliasBodies: Map[String, Type] = Map(
    "string" -> ListType(charType),
    "boolean" -> UnionType(Set(falseType, trueType)),
    "timeout" -> UnionType(Set(AtomLitType("infinity"), NumberType)),
    "identifier" -> UnionType(Set(PidType, PortType, ReferenceType)),
    "mfa" -> TupleType(List(AtomType, AtomType, NumberType)),
    "iolist" -> ListType(UnionType(Set(byteType, BinaryType, RemoteType(ioListRid, Nil)))),
    "iodata" -> UnionType(Set(RemoteType(RemoteId("erlang", "iolist", 0), Nil), BinaryType)),
  )

  val builtinTypeAliases: Map[String, Type] =
    builtinTypeAliasBodies.keySet.map(name => name -> RemoteType(RemoteId("erlang", name, 0), Nil)).toMap

  val stringType: Type = builtinTypeAliases("string")
  val booleanType: Type = builtinTypeAliases("boolean")

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

  def recordAsTuple(recDecl: RecDecl): TupleType = {
    val elems = AtomLitType(recDecl.name) :: recDecl.fields.map(_.tp)
    TupleType(elems)
  }

  def refinedRecordAsTuple(recDecl: RecDecl, refinedRecord: RefinedRecordType): TupleType = {
    val fieldsList = recDecl.fields.map(f => refinedRecord.fields.getOrElse(f.name, f.tp))
    TupleType(AtomLitType(refinedRecord.recType.name) :: fieldsList)
  }

  val builtinTypes: Map[String, Type] =
    Map(
      "any" -> AnyType,
      "atom" -> AtomType,
      "binary" -> BinaryType,
      "dynamic" -> DynamicType,
      "nonempty_binary" -> BinaryType,
      "bitstring" -> BinaryType,
      "nonempty_bitstring" -> BinaryType,
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

  implicit val codec: JsonValueCodec[Type] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  object Key {
    def asType(k: Key): Type = {
      k match {
        case TupleKey(keys) => TupleType(keys.map(asType))
        case AtomKey(atom)  => AtomLitType(atom)
      }
    }

    def fromType(t: Type): Option[Key] = {
      t match {
        case AtomLitType(atom) => Some(AtomKey(atom))
        case TupleType(kts) =>
          boundary {
            var keys: List[Key] = List()
            for (kt <- kts) {
              fromType(kt) match {
                case None      => boundary.break(None)
                case Some(key) => keys = keys :+ key
              }
            }
            Some(TupleKey(keys))
          }
        case _ => None
      }
    }

    def fromExpr(e: Expr): Option[Key] = {
      e match {
        case AtomLit(atom) => Some(AtomKey(atom))
        case Tuple(exprs) =>
          boundary {
            var keys: List[Key] = List()
            for (expr <- exprs) {
              fromExpr(expr) match {
                case None    => boundary.break(None)
                case Some(k) => keys = keys :+ k
              }
            }
            Some(TupleKey(keys))
          }
        case _ => None
      }
    }

    def fromTest(t: Test): Option[Key] = {
      t match {
        case TestAtom(atom) => Some(AtomKey(atom))
        case TestTuple(exprs) =>
          boundary {
            var keys: List[Key] = List()
            for (expr <- exprs) {
              fromTest(expr) match {
                case None    => boundary.break(None)
                case Some(k) => keys = keys :+ k
              }
            }
            Some(TupleKey(keys))
          }
        case _ => None
      }
    }

    private def parse(str: String): Key = {
      def split(str: String): List[String] = {
        if (str.isEmpty) return List()
        var cur = ""
        var inBraces = 0
        var res: List[String] = List()
        for (c <- str) {
          if (c == '{') inBraces += 1
          else if (c == '}') inBraces -= 1
          if (c == ',' && inBraces == 0) {
            res = cur.strip() :: res
            cur = ""
          } else {
            cur = cur + c
          }
        }
        res = cur.strip() :: res
        res.reverse
      }
      if (str.nonEmpty && str.charAt(0) == '{') {
        val substrs = split(str.substring(1, str.length - 1))
        TupleKey(substrs.map(parse))
      } else {
        AtomKey(str)
      }
    }

    implicit val keyCodec: JsonKeyCodec[Key] = new JsonKeyCodec[Key] {
      override def decodeKey(in: JsonReader): Key = {
        val key = in.readKeyAsString()
        parse(key)
      }

      override def encodeKey(k: Key, out: JsonWriter): Unit = {
        out.writeKey(k.toString)
      }
    }
  }
}
