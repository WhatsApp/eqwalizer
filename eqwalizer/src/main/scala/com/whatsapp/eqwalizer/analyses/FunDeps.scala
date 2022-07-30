/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{App, RemoteId}
import com.whatsapp.eqwalizer.config
import com.whatsapp.eqwalizer.ast.stub.DbApi

// $COVERAGE-OFF$
object FunDeps {
  type Deps = Map[RemoteId, List[RemoteId]]
  type Graph = Map[RemoteId, Set[RemoteId]]

  def main(args: Array[String]): Unit = {
    val genMods = DbApi.projectApps.values.flatMap(_.modules).filter(DbApi.isGenerated).toSet
    val rawDeps = getDeps()
    val allDeps = rawDeps.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    val genDeps = allDeps.view.filterKeys(rid => genMods(rid.module)).toMap.withDefaultValue(Set.empty)
    val nonGenDeps = allDeps.view.filterKeys(rid => !genMods(rid.module)).toMap.withDefaultValue(Set.empty)
    val nonGenWithoutThriftDeps =
      allDeps.view
        .filterKeys(rid => !rid.module.startsWith("thrift") && !genMods(rid.module))
        .toMap
        .withDefaultValue(Set.empty)

    analyzeDeps("All code", allDeps)
    analyzeDeps("Generated code", genDeps)
    analyzeDeps("NonGenerated code", nonGenDeps)
    analyzeDeps("NonGenerated code (Without thrift_)", nonGenWithoutThriftDeps)
  }

  def analyzeDeps(config: String, g: Graph): Unit = {
    val components = SCC.computeSCC(g)
    val allFuns = g.size
    val mutRecursiveSCCs = components.filter(_.size > 1)
    val mutRecursiveFuns = mutRecursiveSCCs.map(_.size).sum
    val mutRecursiveRatio = if (allFuns == 0) 0 else mutRecursiveFuns * 100 / allFuns
    val report =
      s"""
         | Configuration: $config
         |                                    All funs : $allFuns
         |                            Non-trivial SCCs : ${mutRecursiveSCCs.size}
         |                     Mutually recursive funs : $mutRecursiveFuns
         |                      Mutual recursion ratio : $mutRecursiveRatio%
         |--------------------------------------------
         |""".stripMargin
    Console.println(report)
  }

  private def getDeps(): Deps =
    DbApi.projectApps.values.toList.sortBy(_.name).map(appDeps).foldLeft(Map.empty: Deps)(_ ++ _)

  private def appDeps(app: App): Deps =
    if (config.depApps(app.name))
      Map.empty
    else
      app.modules.map(moduleDeps).foldLeft(Map.empty: Deps)(_ ++ _)

  private def moduleDeps(module: String): Deps =
    ModuleFunDeps.getFunDeps(module)
}
// $COVERAGE-ON$
