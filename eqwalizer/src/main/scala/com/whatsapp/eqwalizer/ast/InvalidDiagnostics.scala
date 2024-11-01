/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types.Type
import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

object InvalidDiagnostics {
  sealed trait Invalid extends Exception with Diagnostic {
    override def erroneousExpr: Option[Exprs.Expr] = None
  }
  case class UnknownId(pos: Pos, id: RemoteId) extends Invalid {
    val msg: String = s"Unknown id: $id"
    def errorName = "unknown_id"
  }
  case class NonExportedId(pos: Pos, id: RemoteId) extends Invalid {
    val msg: String = s"Type exists but is not exported: $id"
    def errorName = "non_exported_id"
  }
  case class RecursiveConstraint(pos: Pos, n: String) extends Invalid {
    val msg: String = s"Recursive constraint: $n"
    def errorName = "recursive_constraint"
  }
  case class TyVarWithMultipleConstraints(pos: Pos, n: String) extends Invalid {
    val msg: String = s"Type variable '$n' is constrained multiple times, please remove the extra constraints"
    def errorName = "ty_var_with_multiple_constraints"
  }
  case class TypeVarInRecordField(pos: Pos, name: String) extends Invalid {
    val msg: String = {
      s"$name: Type variables are meaningless in record fields. Did you mean to use an alias?"
    }
    def errorName = "type_var_in_record_field"
  }
  case class UnboundTyVarInTyDecl(pos: Pos, name: String) extends Invalid {
    val msg: String = {
      s"$name: Type variable is unbound."
    }
    def errorName = "unbound_type_var"
  }
  case class RepeatedTyVarInTyDecl(pos: Pos, name: String) extends Invalid {
    val msg: String = s"$name. Type vars in type declarations must be distinct"
    def errorName = "repated_type_var_in_type_decl"
  }
  case class NonProductiveRecursiveTypeAlias(pos: Pos, name: String) extends Invalid {
    val msg: String =
      s"recursive type $name is not productive"
    def errorName = "type_alias_is_non_productive"
  }
  case class TransitiveInvalid(pos: Pos, name: String, references: List[String]) extends Invalid {
    val msg: String = references match {
      case List(ref) =>
        s"$name references type with invalid definition: $ref"
      case Nil =>
        throw new IllegalStateException()
      case refs =>
        s"$name references types with invalid definitions: ${refs.mkString(", ")}"
    }
    def errorName = "reference_to_invalid_type"
  }
  case class AliasWithNonCovariantParam(pos: Pos, name: String, typeVar: String, exps: List[Type])
      extends Exception
      with Invalid {
    val msg: String = {
      val show = new Show(None)
      val expsStr = exps.map(show.show)
      val expands = s"\t$name expands to ${expsStr.head}" :: expsStr.tail.map(exp => s"\twhich expands to ${exp}")
      val explain = s"Opaque $name expands to a type in which $typeVar appears in function parameter position"
      (explain :: expands).mkString("\n")
    }
    def errorName = "type_var_in_parameter_position"
  }
  case class BadMapKey(pos: Pos, required: Boolean) extends Invalid {
    val msg: String = {
      val reason: String = {
        if (required) {
          s"Required map key should always be composed of statically defined atoms or tuples."
        } else {
          s"Only one default association per map is allowed, all other keys should be composed of statically defined atoms or tuples."
        }
      }
      s"Map type will be approximated to #{dynamic() => dynamic()}, suppressing potential errors.\n${reason}"
    }
    def errorName = "bad_map_key"
  }

  implicit val codec: JsonValueCodec[Invalid] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true).withDiscriminatorFieldName(None).withFieldNameMapper {
      case "pos"                     => "location"
      case "mod"                     => "module"
      case s if !s.charAt(0).isUpper => JsonCodecMaker.enforce_snake_case(s)
      case s                         => s
    }
  )
}
