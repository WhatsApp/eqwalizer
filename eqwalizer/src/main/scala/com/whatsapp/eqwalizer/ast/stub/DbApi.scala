/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id

object DbApi {

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

  def getType(module: String, id: Id): Option[TypeDecl] = {
    CustomTypes.getType(module, id).orElse {
      Db.getModuleStub(module).flatMap(_.types.get(id))
    }
  }

  def getOpaque(module: String, id: Id): Option[TypeDecl] =
    Db.getModuleStub(module).flatMap(_.opaques.get(id))

  def getRecord(module: String, record: String): Option[RecDecl] =
    Db.getModuleStub(module).flatMap(_.records.get(record))

  def getInvalidForms(module: String): Option[List[InvalidForm]] =
    Db.getModuleStub(module).map(_.invalidForms)

  def isKnownModule(module: String): Boolean =
    Db.getModuleStub(module).isDefined

  def loadedModules(): Set[String] =
    Db.getLoadedModules()
}
