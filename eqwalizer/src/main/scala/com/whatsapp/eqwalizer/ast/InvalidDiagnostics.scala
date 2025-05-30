/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}

object InvalidDiagnostics {
  sealed trait Invalid extends Diagnostic {
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
  case class BadMapKey(pos: Pos, required: Boolean) extends Invalid {
    val msg: String = {
      val reason: String = {
        if (required) {
          s"Use => instead of := here. Required map key should always be composed of statically defined atoms or tuples."
        } else {
          s"Only one default association per map is allowed, all other keys should be composed of statically defined atoms or tuples."
        }
      }
      s"Map type will be approximated to #{dynamic() => dynamic()}, suppressing potential errors.\n${reason}"
    }
    def errorName = "bad_map_key"
  }
  case class InvalidRefInTypeCast(pos: Pos, references: List[String]) extends Invalid {
    val msg: String = references match {
      case List(ref) =>
        s"type cast references type with invalid definition: $ref"
      case Nil =>
        throw new IllegalStateException()
      case refs =>
        s"type cast references types with invalid definitions: ${refs.mkString(", ")}"
    }

    def errorName = "invalid_ref_in_type_cast"
  }
  case class VariablesInTypeCast(pos: Pos, variables: List[String]) extends Invalid {
    val msg: String = s"type variables are not allowed in type casts, found: ${variables.mkString(", ")}"

    def errorName = "variables_in_type_cast"
  }
  case class RefinedRecordInTypeCast(pos: Pos, name: String) extends Invalid {
    val msg: String = s"refined records are not allowed in type casts, found: $name"

    def errorName = "refined_record_in_type_cast"
  }

  implicit val codec: JsonValueCodec[Invalid] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )
}
