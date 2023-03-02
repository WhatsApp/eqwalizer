/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, Forms}

object FactorizableUnions {
  private val listener = new FactorizableUnionsListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val results = listener.getResults.sortWith(_._2 < _._2)
    var moreThanOne = 0
    results.foreach { case (location, facts, union) =>
      Console.println(s"${location} --- ${facts} factorization(s)")
      Console.println(s"${union}")
      if (facts > 1) {
        moreThanOne += 1
      }
    }
    Console.println(s"Total factorizable unions: ${results.size}")
    Console.println(s"Factorizable more than once: ${moreThanOne}")
  }

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.foreach(analyzeApp)
  }

  def analyzeApp(app: App): Unit =
    app.modules.sorted.foreach(analyzeModule)

  def analyzeModule(module: String): Unit = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    Pipeline.traverseForms(forms, listener)
  }
}
