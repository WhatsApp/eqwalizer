/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.App
import com.whatsapp.eqwalizer.ast.stub.DbApi

object BadPropTypes {
  private val listener = new BadPropTypesListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val locations = listener.getLocations.distinct.sorted
    locations.foreach { Console.println }
    Console.println(s"Total: ${locations.size}")
  }

  private def analyze(): Unit = {
    // analyzeApps(DB.otpApps.values.toList)
    // analyzeApps(DB.depApps.values.toList)
    analyzeApps(DbApi.projectApps.values.toList)
  }

  private def analyzeApps(apps: List[App]): Unit =
    apps.sortBy(_.name).foreach(analyzeApp)

  def analyzeApp(app: App): Unit =
    app.modules.sorted.foreach(analyzeModule)

  def analyzeModule(module: String): Unit = {
    val forms = DbApi.loadStubForms(module).get
    Pipeline.traverseForms(forms, listener)
  }
}
