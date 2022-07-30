/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.BehaviourError

object BehaviourErrors {
  // $COVERAGE-OFF$
  def main(args: Array[String]): Unit = {
    val results = analyzeApps(DbApi.projectApps.values.toList)
    for ((module, errors) <- results if errors.nonEmpty) {
      println(module)
      println(errors.map(_.msg).mkString("\n"))
    }
  }

  private def analyzeApps(apps: List[App]): Map[String, List[BehaviourError]] =
    apps.flatMap(analyzeApp).toMap

  private def analyzeApp(app: App): Map[String, List[BehaviourError]] =
    app.modules.sorted.map(m => m -> analyzeModule(m)).toMap

  private def analyzeModule(module: String): List[BehaviourError] = {
    val astStorage = DbApi.getAstStorage(module).get
    Pipeline.checkForms(astStorage).flatMap {
      case Forms.MisBehaviour(te) => List(te)
      case _                      => Nil
    }
  }
  // $COVERAGE-ON$
}
