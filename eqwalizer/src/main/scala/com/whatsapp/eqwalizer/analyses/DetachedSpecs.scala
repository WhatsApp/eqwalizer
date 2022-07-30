/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.{Pipeline, config}

object DetachedSpecs {
  private val listener = new DetachedSpecsListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val locations = listener.getLocations
    locations.map(l => s"${l.source}:${l.line}").foreach(Console.println)
    Console.println(s"Total: ${locations.size} detached specs")
  }

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.foreach(analyzeApp)
  }

  def analyzeApp(app: App): Unit =
    if (!config.depApps(app.name)) {
      app.modules.foreach(analyzeModule)
    }

  def analyzeModule(module: String): Unit = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    Pipeline.traverseForms(forms, listener)
  }
}
