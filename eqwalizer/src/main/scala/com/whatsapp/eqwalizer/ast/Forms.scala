/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs.{Clause, Expr}
import com.whatsapp.eqwalizer.ast.Types._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.whatsapp.eqwalizer.io.Ipc

object Forms {

  sealed trait InternalForm { val pos: Pos }

  case class Module(name: String)(val pos: Pos) extends InternalForm
  case class FunDecl(id: Id, clauses: List[Clause])(val pos: Pos) extends InternalForm
  case class File(file: String, start: Int)(val pos: Pos) extends InternalForm
  case class Fixme(comment: TextRange, suppression: TextRange, isIgnore: Boolean)
  case class ElpMetadata(fixmes: List[Fixme])(val pos: Pos) extends InternalForm
  case class Behaviour(name: String)(val pos: Pos) extends InternalForm
  case class EqwalizerNowarnFunction(id: Id)(val pos: Pos) extends InternalForm
  case class EqwalizerUnlimitedRefinement(id: Id)(val pos: Pos) extends InternalForm

  case class FunSpec(id: Id, ty: FunType)
  case class OverloadedFunSpec(id: Id, tys: List[FunType])
  // empty tys list is used to represent callback with an invalid type
  case class Callback(id: Id, tys: List[FunType])
  case class RecDecl(name: String, fields: List[RecField], refinable: Boolean) {
    lazy val fMap: Map[String, RecField] = fields.map(f => f.name -> f).toMap
  }
  case class RecField(name: String, tp: Type, defaultValue: Option[Expr], refinable: Boolean)
  case class TypeDecl(id: Id, params: List[VarType], body: Type)

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
