/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.Forms.FuncDecl
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.TcDiagnostics.ExpectedSubtype

object LongErrors {

  private val maxVerboseResults = 100
  private var verbose = false
  case class Mismatch(rid: RemoteId, msg: String)

  def main(args: Array[String]): Unit = {
    verbose = args.contains("-v")
    val mismatches = analyzeApps(DbApi.projectApps.values.toList)
    printSummary(mismatches)
  }

  private def printSummary(mismatches: List[Mismatch]): Unit = {
    val results = mismatches.sortBy(_.msg.size)
    if (verbose) {
      val topResults = results.take(results.size.max(maxVerboseResults))
      for (Mismatch(rid, msg) <- topResults)
        Console.println(s"\n\n$rid size ${msg.size}\n$msg")
    }
    val sizes = results.map(_.msg.size)
    val avg = sizes.sum.toFloat / results.size
    println(s"Average error message length: $avg characters")
    Console.println(s"Longest error message: ${sizes.last} characters")
    val topPercentile = results.takeRight(results.size / 10)
    val topPercentileAvg = topPercentile.map(_.msg.size).sum.toFloat / topPercentile.size
    println(s"Average error message length for the longest 10% of error msgs: $topPercentileAvg characters")
  }

  private def analyzeApps(apps: List[App]): List[Mismatch] =
    apps.sortBy(_.name).flatMap(analyzeApp)

  private def analyzeApp(app: App): List[Mismatch] =
    app.modules.sorted.flatMap(analyzeModule)

  private def analyzeModule(module: String): List[Mismatch] = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Pipeline.checkForms(astStorage)
    forms.flatMap {
      case FuncDecl(Id(name, arity), errors) =>
        errors.collect { case te: ExpectedSubtype =>
          val noExplanationMsg = te.msg.linesIterator.next()
          Mismatch(RemoteId(module, name, arity), noExplanationMsg)
        }
      case _ =>
        Nil
    }
  }
}
