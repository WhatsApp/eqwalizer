/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{AstListener, RemoteId}

import scala.collection.mutable.ListBuffer

class TypeDepsListener extends AstListener {
  def getDeps: Map[RemoteId, List[RemoteId]] = deps
  private var currentType: Option[RemoteId] = None
  private var module: Option[String] = None
  private val currentDeps: ListBuffer[RemoteId] = ListBuffer.empty
  private var deps: Map[RemoteId, List[RemoteId]] = Map.empty

  override def enterModule(m: String, erlFile: String): Unit =
    module = Some(m)

  override def enterTypeDecl(tp: ExternalTypeDecl): Unit =
    currentType = Some(RemoteId(module.get, tp.id.name, tp.id.arity))
  override def exitTypeDecl(tp: ExternalTypeDecl): Unit = {
    deps += currentType.get -> currentDeps.toList
    currentType = None
    currentDeps.clear()
  }

  override def enterOpaqueDecl(tp: ExternalOpaqueDecl): Unit =
    currentType = Some(RemoteId(module.get, tp.id.name, tp.id.arity))
  override def exitOpaqueDecl(tp: ExternalOpaqueDecl): Unit = {
    deps += currentType.get -> currentDeps.toList
    currentType = None
    currentDeps.clear()
  }

  override def enterRecDecl(recDecl: ExternalRecDecl): Unit =
    // "hack" to map a record to FQN
    currentType = Some(RemoteId(module.get, recDecl.name, ModuleTypeDeps.recordArity))

  override def exitRecDecl(recDecl: ExternalRecDecl): Unit = {
    deps += currentType.get -> currentDeps.toList
    currentType = None
    currentDeps.clear()
  }

  override def enterType(tp: ExtType): Unit =
    if (currentType.isDefined)
      tp match {
        case LocalExtType(lid, _) =>
          currentDeps += RemoteId(module.get, lid.name, lid.arity)
        case RemoteExtType(rid, _) =>
          currentDeps += rid
        case RecordExtType(name) =>
          currentDeps += RemoteId(module.get, name, ModuleTypeDeps.recordArity)
        case RecordRefinedExtType(name, _) =>
          currentDeps += RemoteId(module.get, name, ModuleTypeDeps.recordArity)
        case _ =>
      }

  override def exitType(tp: ExtType): Unit =
    ()
}
