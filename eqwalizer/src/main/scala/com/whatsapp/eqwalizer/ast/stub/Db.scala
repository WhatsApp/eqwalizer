/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid

import scala.collection.mutable
import com.whatsapp.eqwalizer.io.Ipc

object Db {
  private val stubs: mutable.Map[String, Option[ModuleStub]] = mutable.Map.empty
  private val loadedModules: mutable.Set[String] = mutable.Set.empty

  /** module stub suitable for type-checking
    */
  def getModuleStub(module: String): Option[ModuleStub] = {
    loadedModules.add(module)
    if (stubs.contains(module)) stubs(module)
    else {
      val optStub =
        Ipc.getAstBytes(module, Ipc.TransitiveStub).map(readFromArray[ModuleStub](_))
      stubs.put(module, optStub)
      optStub
    }
  }

  def getLoadedModules(): Set[String] = {
    val set = loadedModules.toSet
    loadedModules.clear()
    set ++ Set("eqwalizer_types", "eqwalizer_specs")
  }

  def getCallbacks(module: String): (List[Callback], Set[Id]) =
    getModuleStub(module) match {
      case Some(stub) =>
        (stub.callbacks, stub.optionalCallbacks)
      case None =>
        (List.empty, Set.empty)
    }

  def getSpec(module: String, id: Id): Option[FunSpec] =
    if (CustomSpecs.getOverloadedSpec(module, id).isDefined) None
    else
      CustomSpecs.getSpec(module, id).orElse {
        getModuleStub(module).flatMap(_.specs.get(id))
      }

  def getOverloadedSpec(module: String, id: Id): Option[OverloadedFunSpec] =
    if (CustomSpecs.getSpec(module, id).isDefined)
      None
    else
      CustomSpecs.getOverloadedSpec(module, id).orElse {
        getModuleStub(module).flatMap(_.overloadedSpecs.get(id))
      }

  def isExported(module: String, id: Id): Boolean =
    getModuleStub(module).exists(_.exports(id))

  def getImports(module: String): Option[Map[Id, String]] =
    getModuleStub(module).map(_.imports)

  def getType(module: String, id: Id): Option[TypeDecl] = {
    CustomTypes.getType(module, id).orElse {
      getModuleStub(module).flatMap(_.types.get(id))
    }
  }

  def getOpaque(module: String, id: Id): Option[TypeDecl] =
    getModuleStub(module).flatMap(_.opaques.get(id))

  def getRecord(module: String, record: String): Option[RecDecl] =
    getModuleStub(module).flatMap(_.records.get(record))

  def getInvalids(module: String): Option[List[Invalid]] =
    getModuleStub(module).map(_.invalids)

  def isKnownModule(module: String): Boolean =
    getModuleStub(module).isDefined
}
