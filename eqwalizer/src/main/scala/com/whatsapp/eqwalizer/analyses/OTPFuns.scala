/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, Forms}

object OTPFuns {
  private val listener = new OTPFunsListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val infos = listener.getInfos
    val perModule = DbApi.otpModules.toList
      .flatMap { module =>
        val count = infos.count { case (_, rid) => rid.module == module }
        if (count > 0) Some(count, module)
        else None
      }
      .sortBy(_._1)
    val perApp = DbApi.otpApps.toList
      .flatMap { case (_, app) =>
        val count = infos.count { case (_, rid) => app.modules.contains(rid.module) }
        if (count > 0) Some(count, app)
        else None
      }
      .sortBy(_._1)
    Console.println("Per module stats:")
    perModule.foreach { case (count, module) => Console.println(s"${module}  ${count}") }
    Console.println("Per app stats:")
    perApp.foreach { case (count, app) => Console.println(s"${app.name}  ${count}") }
  }

  private def analyze(): Unit = {
    analyzeApps(DbApi.projectApps.values.toList)
  }

  private def analyzeApps(apps: List[App]): Unit =
    apps.sortBy(_.name).foreach(analyzeApp)

  def analyzeApp(app: App): Unit =
    app.modules.sorted.foreach(analyzeModule)

  def analyzeModule(module: String): Unit = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    Pipeline.traverseForms(forms, listener)
  }
}
