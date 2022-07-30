/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{App, RemoteId}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.config

// $COVERAGE-OFF$
object TypeDeps {
  type Deps = Map[RemoteId, List[RemoteId]]
  type Graph = Map[RemoteId, Set[RemoteId]]

  def main(args: Array[String]): Unit = {
    val otp = args.contains("-otp")
    val rawDeps = getDeps(otp)
    val allDeps = rawDeps.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    analyzeDeps(allDeps)
  }

  def analyzeDeps(g: Graph): Unit = {
    val components = SCC.computeSCC(g)
    val recComponents = components.filter { comp =>
      comp.size > 1 || g(comp.head)(comp.head)
    }
    for (recComponent <- recComponents) {
      println(recComponent.mkString(", "))
      // all rec types are inside the same module
      assert(recComponent.map(_.module).size == 1)
    }
    val paramRecComponents = recComponents.filter(_.exists(_.arity >= 1))
    if (paramRecComponents.nonEmpty) {
      println("PARAMETERIZED REC TYPES")
      for (recComponent <- paramRecComponents)
        println(recComponent.mkString(", "))
    }
  }

  private def getDeps(otp: Boolean): Deps = {
    val apps = if (otp) DbApi.otpApps else DbApi.projectApps
    apps.values.toList.sortBy(_.name).map(appDeps).foldLeft(Map.empty: Deps)(_ ++ _)
  }

  private def appDeps(app: App): Deps =
    if (config.depApps(app.name))
      Map.empty
    else
      app.modules.map(moduleDeps).foldLeft(Map.empty: Deps)(_ ++ _)

  private def moduleDeps(module: String): Deps = {
    println(module)
    ModuleTypeDeps.getTypeDeps(module)
  }
}
// $COVERAGE-ON$
