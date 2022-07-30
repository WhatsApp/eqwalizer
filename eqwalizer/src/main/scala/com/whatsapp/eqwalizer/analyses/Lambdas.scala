/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import java.nio.file.{Files, Paths}
import com.whatsapp.eqwalizer.{Pipeline, config}
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.ast.stub.DbApi

object Lambdas {
  import LambdasListener.LambdaInfo
  val listener = new LambdasListener
  private var verbose = false

  def main(args: Array[String]): Unit = {
    verbose = args.contains("-v")
    analyze()
    printLambdaSummary()
  }

  private def printLambdaSummary(): Unit = {
    val a = listener.analysis
    // $COVERAGE-OFF$
    val uncategorized =
      (a.lambdas -- a.fieldLambdas -- a.varLambdas -- a.callLambdas -- a.argLambdas -- a.tupleLambdas -- a.mapLambdas -- a.lastInCaseClauseLambdas -- a.lastInFunDeclLambdas).toList
    if (verbose) {
      val sep = "==============="
      def pad(s: String): String = s.padTo(15, ' ')
      def printVerbose(title: String, infos: List[LambdaInfo]): Unit = {
        println("\n ")
        println(title)
        println(sep)
        if (infos.isEmpty) println("    none found")
        else {
          infos.groupBy(_.module).foreach { case (module, lambdas) =>
            val erlFile = a.moduleToFile(module)
            val lines = new String(Files.readAllBytes(Paths.get(erlFile))).linesIterator.toVector
            lambdas.foreach(li => println(s"    ${pad(li.module)} line ${li.line} ${lines(li.line - 1)}"))
          }
        }
      }
      printVerbose("lambdas directly as record fields", a.fieldLambdas)
      printVerbose("lambdas assigned directly to variables", a.varLambdas)
      printVerbose("lambdas assigned directly to variables used once", a.varLambdas.filterNot(a.varLambdasUsedMulti))
      printVerbose("lambdas called directly", a.callLambdas)
      printVerbose("lambdas called directly (non-macro-lines)", a.callLambdas.filterNot(a.macroLineLambdas))
      printVerbose("lambdas directly as arguments", a.argLambdas)
      printVerbose("lambdas directly as tuple elements", a.tupleLambdas)
      printVerbose("lambdas directly as map values", a.mapLambdas)
      printVerbose("lambdas last-in-case-clause", a.lastInCaseClauseLambdas)
      printVerbose("lambdas last-in-fun-decl", a.lastInFunDeclLambdas)
      printVerbose("uncategorized", uncategorized)
    }
    // $COVERAGE-ON$
    val lambdaCount = a.lambdas.size
    println(s"\n \nlambda count: $lambdaCount")
    println(s"\n \nnamed lambda count: ${a.lambdas.filter(_.isNamed).size}")
    printStat("lambdas directly as record fields", a.fieldLambdas.size, lambdaCount, "of lambdas")
    printStat(
      "lambdas assigned directly to variables",
      a.varLambdas.size,
      lambdaCount,
      "of lambdas",
    )
    printStat(
      "lambdas assigned directly to variables used once",
      a.varLambdas.filterNot(a.varLambdasUsedMulti).size,
      lambdaCount,
      "of lambdas",
    )
    printStat("lambdas called directly", a.callLambdas.size, lambdaCount, "of lambdas")
    printStat("lambdas directly as arguments", a.argLambdas.size, lambdaCount, "of lambdas")
    printStat("lambdas directly as tuple elements", a.tupleLambdas.size, lambdaCount, "of lambdas")
    printStat("lambdas directly as map values", a.mapLambdas.size, lambdaCount, "of lambdas")
    printStat("lambdas last-in-case-clause", a.lastInCaseClauseLambdas.size, lambdaCount, "of lambdas")
    printStat("lambdas last-in-fun-decl", a.lastInFunDeclLambdas.size, lambdaCount, "of lambdas")
    println("\n \n ")
    val nonGenCount = a.lambdas.filterNot(a.generatedLambdas).size
    println(s"\n\nnon-generated lambda count: $nonGenCount")
    printStat(
      "non-generated lambdas directly as record fields",
      a.fieldLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas assigned directly to variables",
      a.varLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas assigned directly to variables used once",
      a.varLambdas.filterNot(a.generatedLambdas).filterNot(a.varLambdasUsedMulti).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas called directly",
      a.callLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas directly as arguments",
      a.argLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas directly as tuple elements",
      a.tupleLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated lambdas directly as map values",
      a.mapLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated last-in-case-clause lambdas",
      a.lastInCaseClauseLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    printStat(
      "non-generated last-in-fun-decl lambdas",
      a.lastInFunDeclLambdas.filterNot(a.generatedLambdas).size,
      nonGenCount,
      "of non-generated lambdas",
    )
    val genMacroLambdas = a.generatedLambdas ++ a.macroLineLambdas
    val nonGenNonMacroCount = a.lambdas.filterNot(genMacroLambdas).size
    println("\n \n ")
    println(s"\n\nnon-generated lambdas not appearing on lines with macros count: $nonGenNonMacroCount")
    printStat(
      "non-generated non-macro-line lambdas directly as record fields",
      a.fieldLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas assigned directly to variables",
      a.varLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas assigned directly to variables used once",
      a.varLambdas.filterNot(genMacroLambdas).filterNot(a.varLambdasUsedMulti).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas called directly",
      a.callLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas directly as arguments",
      a.argLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas directly as tuple elements",
      a.tupleLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line lambdas directly as map values",
      a.mapLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line last-in-case-clause lambdas",
      a.lastInCaseClauseLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    printStat(
      "non-generated non-macro-line last-in-fun-decl lambdas",
      a.lastInFunDeclLambdas.filterNot(genMacroLambdas).size,
      nonGenNonMacroCount,
      "of non-generated non-macro-line lambdas",
    )
    println(
      s"\n \n All uncategorized lambdas: ${uncategorized.size} (${(uncategorized.size * 100.0 / lambdaCount).toInt}% of all lambdas)"
    )
    println("\n ")
    println("""
           |
           | Methodology notes:
           | - "lambda called directly" means code like:  `(fun(X) -> X end)(2)`. These typically only
           | exist when created via macros such as `-define(LAZY(X), fun() -> X end).`
           | - a "non-macro-line-lambda" is a lambda *not* on the same line as a macro. This is helpful for seeing
           | stats that don't include lambdas generated from macros.
           | - A "last-in-case-clause" lambda is a lambda that is the last expression in a `case` clause.
           | - stats do not include named fun expressions, for which there are around 26 in waserver as of 2021-05-06,
           |    case-sensitive regex: `[^\w]fun\s+\w+\s*\(`. 10 of the named fun expressions are recursive.
           | - run this analysis with `-v` for verbose output, including all uncategorized uses of lambdas.
    """.stripMargin)
  }

  private def printStat(description: String, numerator: Int, denominator: Int, pctExplanation: String): Unit = {
    val pct = if (denominator == 0) 0 else (numerator * 100.0 / denominator).toInt
    Console.println(
      s"$description: $numerator ($pct% of $pctExplanation)"
    )
  }

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.foreach(analyzeApp)
  }

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
