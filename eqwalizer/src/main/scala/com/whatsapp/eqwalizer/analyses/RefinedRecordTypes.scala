/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.App
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.{Pipeline, config}

object RefinedRecordTypes {
  private val listener = new RefinedRecordTypesListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val locations = listener.getLocations
    locations.foreach(Console.println)
    Console.println(s"Total: ${locations.size}")
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
    val forms = DbApi.loadStubForms(module).get
    Pipeline.traverseForms(forms, listener)
  }
}
