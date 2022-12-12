/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.ericsson.otp.erlang._
import com.whatsapp.eqwalizer.ast.Exprs.{Clause, Expr}
import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{BehaviourError, TypeError}

import scala.collection.immutable.TreeSeqMap

object Forms {

  case class Module(name: String)(val pos: Pos) extends ExternalForm with InternalForm
  case class CompileExportAll()(val pos: Pos) extends ExternalForm
  case class Export(funs: List[Id])(val pos: Pos) extends ExternalForm with InternalForm
  case class Import(module: String, funs: List[Id])(val pos: Pos) extends ExternalForm with InternalForm
  case class ExportType(types: List[Id])(val pos: Pos) extends ExternalForm with InternalForm
  case class FunDecl(id: Id, clauses: List[Clause])(val pos: Pos) extends ExternalForm with InternalForm
  case class File(file: String, start: Int)(val pos: Pos) extends ExternalForm with InternalForm
  case class Fixme(comment: TextRange, suppression: TextRange)
  case class ElpMetadata(fixmes: List[Fixme])(val pos: Pos) extends ExternalForm with InternalForm
  case class Behaviour(name: String)(val pos: Pos) extends ExternalForm with InternalForm
  case class EqwalizerNowarnFunction(id: Id)(val pos: Pos) extends ExternalForm with InternalForm
  case class EqwalizerUnlimitedRefinement(id: Id)(val pos: Pos) extends ExternalForm with InternalForm

  /** used for analyses only, should not affect the behavior of the type checker
   */
  case class TypingAttribute(names: List[String])(val pos: Pos) extends ExternalForm

  sealed trait Form

  sealed trait ExternalForm extends Form { val pos: Pos }
  case class ExternalTypeDecl(id: Id, params: List[String], body: ExtType)(val pos: Pos) extends ExternalForm
  case class ExternalOpaqueDecl(id: Id, params: List[String], body: ExtType)(val pos: Pos) extends ExternalForm
  case class ExternalFunSpec(id: Id, types: List[ConstrainedFunType])(val pos: Pos) extends ExternalForm
  case class ExternalCallback(id: Id, types: List[ConstrainedFunType])(val pos: Pos) extends ExternalForm
  case class ExternalOptionalCallbacks(ids: List[Id])(val pos: Pos) extends ExternalForm
  case class ExternalRecDecl(name: String, fields: List[ExternalRecField])(val pos: Pos) extends ExternalForm
  case class ExternalRecField(name: String, tp: Option[ExtType], defaultValue: Option[Expr])

  sealed trait InternalForm extends Form { val pos: Pos }
  case class FunSpec(id: Id, ty: FunType)(val pos: Pos) extends InternalForm
  case class OverloadedFunSpec(id: Id, tys: List[FunType])(val pos: Pos) extends InternalForm

  // empty tys list is used to represent callback with an invalid type
  case class Callback(id: Id, tys: List[FunType])(val pos: Pos) extends InternalForm
  case class RecDecl(name: String, fields: List[RecField], refinable: Boolean)(val pos: Pos) extends InternalForm
  case class RecDeclTyped(name: String, fields: TreeSeqMap[String, RecFieldTyped], refinable: Boolean)
  case class RecField(name: String, tp: Option[Type], defaultValue: Option[Expr], refinable: Boolean)
  case class RecFieldTyped(name: String, tp: Type, defaultValue: Option[Expr], refinable: Boolean)
  case class OpaqueTypeDecl(id: Id)(val pos: Pos) extends InternalForm
  case class TypeDecl(id: Id, params: List[VarType], body: Type)(val pos: Pos) extends InternalForm

  sealed trait UtilForm extends ExternalForm with InternalForm

  sealed trait InvalidForm extends UtilForm {
    val te: TypeError
  }
  case class InvalidTypeDecl(id: Id, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidFunSpec(id: Id, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidRecDecl(name: String, te: Invalid)(val pos: Pos) extends InvalidForm
  case class InvalidConvertTypeInRecDecl(name: String, te: Invalid)(val pos: Pos) extends InvalidForm

  case class NoSpecFuncDecl(id: Id)(val pos: Pos) extends UtilForm
  case class FuncDecl(id: Id, errors: List[TypeError])(val pos: Pos) extends InternalForm
  case class MisBehaviour(te: BehaviourError)(val pos: Pos) extends InternalForm

  def load(astStorage: DbApi.AstStorage): List[ExternalForm] = {
    import com.whatsapp.eqwalizer.io.AstLoader
    import com.whatsapp.eqwalizer.io.EData.EList

    val Some(EList(rawForms, None)) = AstLoader.loadAbstractForms(astStorage)
    val isBeam = astStorage match {
      case _: DbApi.AstBeam => true
      case _                => false
    }
    val noAutoImport = rawForms.flatMap(new ConvertAst(isBeam).extractNoAutoImport).flatten.toSet
    rawForms.flatMap(new ConvertAst(isBeam, noAutoImport).convertForm)
  }

  def isFunForm(o: OtpErlangObject): Boolean =
    o.asInstanceOf[OtpErlangTuple].elementAt(0).equals(new OtpErlangAtom("function"))
}
