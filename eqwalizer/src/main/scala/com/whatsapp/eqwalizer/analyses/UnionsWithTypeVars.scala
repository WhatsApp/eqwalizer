/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.{Pipeline, config}
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.ast.stub.DbApi

object UnionsWithTypeVars {
  val listener = new UnionsWithTypeVarsListener

  def main(args: Array[String]): Unit = {
    if (args.contains("-otp")) DbApi.otpApps.values.foreach(analyzeApp)
    DbApi.depApps.values.foreach(analyzeApp)
    DbApi.projectApps.values.foreach(analyzeApp)
    printSummary()
  }

  private def printSummary(): Unit =
    listener.unionsWithTypeVars.map(_.toString).sorted.foreach(Console.println)

  private def analyzeApp(app: App): Unit =
    if (!config.depApps(app.name)) {
      app.modules.foreach(analyzeModule)
    }

  private def analyzeModule(module: String): Unit = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    Pipeline.traverseForms(forms, listener)
  }
}
