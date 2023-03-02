/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, Forms}

object OverloadingUses {
  private val listener = new OverloadingUsesListener()

  def main(args: Array[String]): Unit = {
    analyze()
    printSummary()
  }

  private def printSummary(): Unit = {
    val infos = listener.getInfos
    val infosNonGen = infos.filter { case (_, info) => !info.generated }
    val overlaps = infosNonGen.filter { case (_, info) => info.overlappingClauses.size > 0 }
    val oneClause = overlaps.filter { case (_, info) => info.clauseCount == 1 }
    val lastClause = overlaps.filter { case (_, info) =>
      info.clauseCount > 1 &&
      info.overlappingClauses.size == 1 &&
      info.overlappingClauses.head._1 == info.clauseCount - 1
    }
    val relevantOverlaps = overlaps.filter { case (_, info) =>
      info.clauseCount > 1 &&
      (info.overlappingClauses.size > 1 ||
        (info.overlappingClauses.size == 1 && info.overlappingClauses.head._1 != info.clauseCount - 1))
    }
    Console.println("--- Overloads with only one clause ---")
    oneClause.foreach { case (remoteId, info) =>
      Console.println(s"${info.location} ${remoteId}")
    }
    Console.println("--- Overloads with spec overlap only on last clause ---")
    lastClause.foreach { case (remoteId, info) =>
      Console.println(s"${info.location} ${remoteId} (${info.overlappingClauses.head._2} overlaps)")
    }
    Console.println("--- Clauses overlaps (nClauses > 1, overlap not in last clause only) ---")
    relevantOverlaps.foreach { case (remoteId, info) =>
      Console.println(s"${info.location} ${remoteId} ${info.overlappingClauses.mkString("(", ", ", ")")}")
    }
    Console.println("--- Statistics summary for non-generated functions ---")
    Console.println(s"Overloaded functions: ${infosNonGen.size}")
    Console.println(s"Functions with clauses overlaps: ${overlaps.size}")
    Console.println(s"Overloaded functions with only one clause: ${oneClause.size}")
    Console.println(s"Overloaded functions with only last-clause overlap: ${lastClause.size}")
    Console.println(s"Remaining overlaps: ${relevantOverlaps.size}")
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
