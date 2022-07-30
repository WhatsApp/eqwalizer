/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.App
import com.whatsapp.eqwalizer.ast.stub.DbApi

object OverloadedFunSpecs {
  private val listener = new OverloadedFunSpecsListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val infos = listener.getInfos
    infos.foreach { info =>
      Console.println(s"${info._1} ${info._2}")
    }
    Console.println(s"Total: ${infos.size}")
  }

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.foreach(analyzeApp)
  }

  def analyzeApp(app: App): Unit =
    app.modules.sorted.foreach(analyzeModule)

  def analyzeModule(module: String): Unit = {
    val forms = DbApi.loadStubForms(module).get
    Pipeline.traverseForms(forms, listener)
  }
}
