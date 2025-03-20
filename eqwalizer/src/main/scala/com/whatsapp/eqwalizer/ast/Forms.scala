/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs.{Clause, Expr}
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{BehaviourError, TypeError}
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.whatsapp.eqwalizer.io.Ipc

import scala.collection.immutable.TreeSeqMap

object Forms {

  sealed trait InternalForm { val pos: Pos }

  case class Module(name: String)(val pos: Pos) extends InternalForm
  case class Export(funs: List[Id])(val pos: Pos) extends InternalForm
  case class Import(module: String, funs: List[Id])(val pos: Pos) extends InternalForm
  case class ExportType(types: List[Id])(val pos: Pos) extends InternalForm
  case class FunDecl(id: Id, clauses: List[Clause])(val pos: Pos) extends InternalForm
  case class File(file: String, start: Int)(val pos: Pos) extends InternalForm
  case class Fixme(comment: TextRange, suppression: TextRange, isIgnore: Boolean)
  case class ElpMetadata(fixmes: List[Fixme])(val pos: Pos) extends InternalForm
  case class Behaviour(name: String)(val pos: Pos) extends InternalForm
  case class EqwalizerNowarnFunction(id: Id)(val pos: Pos) extends InternalForm
  case class EqwalizerUnlimitedRefinement(id: Id)(val pos: Pos) extends InternalForm

  case class FunSpec(id: Id, ty: FunType)(val pos: Pos) extends InternalForm
  case class OverloadedFunSpec(id: Id, tys: List[FunType])(val pos: Pos) extends InternalForm

  // empty tys list is used to represent callback with an invalid type
  case class Callback(id: Id, tys: List[FunType])(val pos: Pos) extends InternalForm
  case class RecDecl(name: String, fields: List[RecField], refinable: Boolean, file: Option[String])(val pos: Pos)
      extends InternalForm
  case class RecDeclTyped(
      name: String,
      fields: TreeSeqMap[String, RecFieldTyped],
      refinable: Boolean,
      file: Option[String],
  )
  case class RecField(name: String, tp: Option[Type], defaultValue: Option[Expr], refinable: Boolean)
  case class RecFieldTyped(name: String, tp: Type, defaultValue: Option[Expr], refinable: Boolean)
  case class TypeDecl(id: Id, params: List[VarType], body: Type, file: Option[String])(val pos: Pos)
      extends InternalForm

  sealed trait InvalidForm extends InternalForm {
    val te: Invalid
  }
  case class InvalidTypeDecl(id: Id, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidFunSpec(id: Id, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidRecDecl(name: String, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidConvertTypeInRecDecl(name: String, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidMapType(te: Invalid)(val pos: Pos) extends InvalidForm

  case class FuncDecl(id: Id, errors: List[TypeError])(val pos: Pos) extends InternalForm
  case class MisBehaviour(te: BehaviourError)(val pos: Pos) extends InternalForm

  def load(module: String): List[InternalForm] = {
    val bytes = Ipc.getAstBytes(module, Ipc.ConvertedForms).get
    readFromArray[List[InternalForm]](bytes)
  }

  implicit val codec: JsonValueCodec[List[InternalForm]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )
}
