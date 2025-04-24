/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import com.whatsapp.eqwalizer.ELPProxy
import com.whatsapp.eqwalizer.ast.Exprs.ExtType
import com.whatsapp.eqwalizer.ast.Forms.*
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.Types.Type

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
    set ++ ELPProxy.depModules() ++ Set("eqwalizer_specs")
  }

  def getCallbacks(module: String): (List[Callback], Set[Id]) =
    getModuleStub(module) match {
      case Some(stub) =>
        (stub.callbacks, stub.optionalCallbacks)
      case None =>
        (List.empty, Set.empty)
    }

  def getSpec(module: String, id: Id): Option[FunSpec] =
    ELPProxy.funSpec(module, id)

  def getOverloadedSpec(module: String, id: Id): Option[OverloadedFunSpec] =
    ELPProxy.overloadedFunSpec(module, id)

  def isExported(module: String, id: Id): Boolean =
    getModuleStub(module).exists(_.exports(id))

  def getImports(module: String): Option[Map[Id, String]] =
    getModuleStub(module).map(_.imports)

  def getType(module: String, id: Id): Option[TypeDecl] =
    ELPProxy.typeDecl(module, id)

  def getOpaque(module: String, id: Id): Option[TypeDecl] =
    ELPProxy.opaqueDecl(module, id)

  def getRecord(module: String, record: String): Option[RecDecl] =
    ELPProxy.recDecl(module, record)

  def getInvalids(module: String): Option[List[Invalid]] =
    getModuleStub(module).map(_.invalids)

  def isKnownModule(module: String): Boolean =
    getModuleStub(module).isDefined

  def validateType(repr: ExtType): Either[Type, Invalid] = {
    val (valid, bytes) = Ipc.validateType(repr)
    if (valid) {
      Left(readFromArray[Type](bytes))
    } else {
      Right(readFromArray[Invalid](bytes))
    }
  }
}
