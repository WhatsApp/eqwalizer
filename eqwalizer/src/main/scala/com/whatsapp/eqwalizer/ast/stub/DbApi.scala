/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{App, ExtModuleStub, Id}
import java.nio.file.Path

object DbApi {

  sealed trait AstStorage
  case class AstBeam(path: Path) extends AstStorage
  case class AstEtfFile(path: Path) extends AstStorage

  /** The AST will be fetched in Erlang Term Format over inter-process communication
    */
  case class AstEtfIpc(module: String) extends AstStorage

  def getCallbacks(module: String): (List[Callback], Set[Id]) =
    Db.getModuleStub(module) match {
      case Some(stub) =>
        (stub.callbacks, stub.optionalCallbacks)
      case None =>
        (List.empty, Set.empty)
    }

  def getSpec(module: String, id: Id): Option[FunSpec] = {
    if (CustomSpecs.getOverloadedSpec(module, id).isDefined)
      None
    else
      CustomSpecs.getSpec(module, id).orElse {
        Db.getModuleStub(module).flatMap(_.specs.get(id))
      }
  }

  def getOverloadedSpec(module: String, id: Id): Option[OverloadedFunSpec] =
    if (CustomSpecs.getSpec(module, id).isDefined)
      None
    else
      CustomSpecs.getOverloadedSpec(module, id).orElse {
        Db.getModuleStub(module).flatMap(_.overloadedSpecs.get(id))
      }

  def isExported(module: String, id: Id): Boolean =
    Db.getModuleStub(module).exists(_.exports(id))

  def getImports(module: String): Option[Map[Id, String]] =
    Db.getModuleStub(module).map(_.imports)

  def getType(module: String, id: Id): Option[TypeDecl] =
    Db.getModuleStub(module).flatMap(_.types.get(id))

  def getPrivateOpaque(module: String, id: Id): Option[TypeDecl] =
    Db.getModuleStub(module).flatMap(_.privateOpaques.get(id))

  def getPublicOpaque(module: String, id: Id): Option[OpaqueTypeDecl] =
    Db.getModuleStub(module).flatMap(_.publicOpaques.get(id))

  def getRecord(module: String, record: String): Option[RecDecl] =
    Db.getModuleStub(module).flatMap(_.records.get(record))

  def getInvalidForms(module: String): Option[List[InvalidForm]] =
    Db.getModuleStub(module).map(_.invalidForms)

  def otpApps: Map[String, App] =
    Db.otpApps

  def projectApps: Map[String, App] =
    Db.projectApps

  def depApps: Map[String, App] =
    Db.depApps

  def apps: Map[String, App] =
    Db.apps

  def loadStubForms(module: String): Option[List[ExternalForm]] =
    Db.loadStubForms(module)

  def getExtModuleStub(module: String): Option[ExtModuleStub] =
    Db.getExtModuleStub(module)

  def getAstStorage(module: String): Option[AstStorage] =
    Db.getAstStorage(module)

  def getBeamAstStorage(module: String): Option[DbApi.AstBeam] =
    Db.getBeamAstStorage(module)

  def isKnownModule(module: String): Boolean =
    Db.getModuleStub(module).isDefined

  def fromBeam(module: String): Boolean =
    Db.fromBeam(module)

  def isGenerated(module: String): Boolean =
    Db.isGenerated(module)

  def isGpbCompileGenerated(module: String): Boolean =
    Db.isGpbCompileGenerated(module)

  def getModuleApp(module: String): Option[App] =
    Db.getModuleApp(module)
}
