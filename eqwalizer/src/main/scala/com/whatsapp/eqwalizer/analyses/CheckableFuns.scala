/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.ExternalForm
import com.whatsapp.eqwalizer.ast.{App, Forms, Id, RemoteId}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.noOptions
import com.whatsapp.eqwalizer.{Pipeline, config}

object CheckableFuns {
  private var verbose = false

  def main(args: Array[String]): Unit = {
    verbose = args.contains("-v")
    val analysis = analyze
    val errorCounts = typeCheck(analysis)
    printSummary(errorCounts, analysis)
  }

  private def typeCheck(analysis: CheckableFunsListener.Analysis): Map[RemoteId, Int] = {
    val funsByMod = analysis.checkable.toList.groupBy(_.module)
    var result: Map[RemoteId, Int] = Map.empty
    for ((module, funs) <- funsByMod) {
      val astStorage = DbApi.getAstStorage(module).get
      val ids = funs.map(rid => Id(rid.name, rid.arity)).toSet
      val subResult = Pipeline.checkFunsAndApplyFixmes(astStorage, ids, noOptions)
      result ++= subResult
    }
    result
  }

  private def printSummary(errorCounts: Map[RemoteId, Int], a: CheckableFunsListener.Analysis): Unit = {
    val funMap = a.checkable.groupBy(_.module)
    val modules = funMap.keys.toList.sorted
    if (verbose)
      // $COVERAGE-OFF$
      for (module <- modules) {
        Console.println(module)
        if (!DbApi.isGpbCompileGenerated(module)) {
          val mfas = funMap(module).toList.sortBy(_.toString)
          mfas.foreach { mfa =>
            Console.println(s"  ${mfa.toString} ${errorCounts(mfa)}")
          }
        }
      }
    val allWellTyped = errorCounts.filter(_._2 == 0).keySet
    // $COVERAGE-ON$
    print("all funs:")
    printCategorySummary(
      allWellTyped = allWellTyped,
      categoryFuns = a.funs,
      checkable = a.checkable ++ allWellTyped,
      errorCounts,
    )
    printSep()
    print("generated funs:")
    val generated = a.funs.diff(a.nonGenerated)
    printCategorySummary(
      allWellTyped = allWellTyped,
      categoryFuns = generated,
      checkable = generated.intersect(a.checkable) ++ generated.intersect(allWellTyped),
      errorCounts,
    )
    printSep()
    print("generated non-test funs:")
    val generatedNonTest = generated.intersect(a.nonTest)
    printCategorySummary(
      allWellTyped = allWellTyped,
      categoryFuns = generatedNonTest,
      checkable = generatedNonTest.intersect(a.checkable) ++ generatedNonTest.intersect(allWellTyped),
      errorCounts,
    )
    printSep()
    print("non-generated funs:")
    printCategorySummary(
      allWellTyped = allWellTyped,
      categoryFuns = a.nonGenerated,
      checkable = a.nonGenerated.intersect(a.checkable) ++ a.nonGenerated.intersect(allWellTyped),
      errorCounts,
    )
    printSep()
    print("non-generated non-test funs (most important):")
    val nonGeneratedNonTestFuns = a.nonGenerated.intersect(a.nonTest)
    printCategorySummary(
      allWellTyped = allWellTyped,
      categoryFuns = nonGeneratedNonTestFuns,
      checkable = nonGeneratedNonTestFuns.intersect(a.checkable) ++ nonGeneratedNonTestFuns.intersect(allWellTyped),
      errorCounts,
    )
  }

  private def printSep(): Unit = Console.println("--------------------------------------------")

  private def printCategorySummary(
      allWellTyped: Set[RemoteId],
      categoryFuns: Set[RemoteId],
      checkable: Set[RemoteId],
      errorCounts: Map[RemoteId, Int],
  ): Unit = {
    val categoryFunsCnt = categoryFuns.size
    val categoryWellTyped = categoryFuns.intersect(allWellTyped)
    val categoryCheckableRatio = ratio(checkable.size, categoryFunsCnt)
    val categoryHealthRatio = ratio(categoryWellTyped.size, checkable.size)
    val errorCount = errorCounts.view.filterKeys(categoryFuns).values.sum

    def pad(s: String): String = s.reverse.padTo(50, ' ').reverse

    val allLabel = pad(s"All")
    val checkableLabel = pad(s"Checkable")
    val wellTypedLabel = pad(s"Well-typed checkable")
    val checkableRatioLabel = pad(s"Checkable ratio")
    val healthLabel = pad(s"Health ratio of checkable")
    val errorCountLabel = pad(s"error count")

    val report =
      s"""
         |$allLabel : $categoryFunsCnt
         |$checkableLabel : ${checkable.size}
         |$wellTypedLabel : ${categoryWellTyped.size}
         |$checkableRatioLabel : $categoryCheckableRatio%
         |$healthLabel : $categoryHealthRatio%
         |$errorCountLabel : $errorCount
         |""".stripMargin
    Console.println(report)
  }

  private def ratio(numerator: Int, denominator: Int): Int = {
    assert(numerator <= denominator)
    if (denominator == 0) 0
    else Math.round(numerator.toFloat * 100 / denominator.toFloat)
  }

  private def analyze: CheckableFunsListener.Analysis = {
    val listener = new CheckableFunsListener()
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.foreach(analyzeApp(_, listener))
    listener.analysis
  }

  private def analyzeApp(app: App, listener: CheckableFunsListener): Unit =
    if (!config.depApps(app.name)) {
      app.modules.foreach(analyzeModule(_, listener))
    }

  private def analyzeModule(module: String, listener: CheckableFunsListener): Unit = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    listener.lambdasAnalysis = toLambdaAnalysis(forms)
    Pipeline.traverseForms(forms, listener)
  }

  private def toLambdaAnalysis(forms: List[ExternalForm]): LambdasListener.Analysis = {
    val lambdasListener = new LambdasListener
    Pipeline.traverseForms(forms, lambdasListener)
    lambdasListener.analysis
  }
}
