/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray

import scala.collection.mutable
import com.whatsapp.eqwalizer.io.Ipc

private object Db {
  private val stubs: mutable.Map[String, Option[ModuleStub]] =
    mutable.Map.empty
  private val loadedModules: mutable.Set[String] = mutable.Set.empty

  /** module stub suitable for type-checking
    */
  def getModuleStub(module: String): Option[ModuleStub] = {
    loadedModules.add(module)
    if (stubs.contains(module))
      stubs(module)
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
    set
  }
}
