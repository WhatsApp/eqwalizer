/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.ast.stub.DbApi

object NullaryUnspecced {
  case class ModuleInfo(module: String, generated: Boolean, privateFuns: List[String], exportedFuns: List[String])

  def main(args: Array[String]): Unit = {
    val infos = analyze().sortBy(_.module)
    printSummary(infos)
  }

  private def printSummary(infos: List[ModuleInfo]): Unit = {
    Console.println("** Manual **")
    val manual = infos.filterNot(_.generated)
    for { i <- manual } if (i.privateFuns.nonEmpty || i.exportedFuns.nonEmpty) {
      Console.println(i.module)
      val all = (i.privateFuns ++ i.exportedFuns).sorted
      val allStr = all.mkString(", ")
      Console.println(s"  $allStr")
    }

    Console.println("** Generated **")
    val gen = infos.filter(_.generated)
    for (i <- gen) if (i.privateFuns.nonEmpty || i.exportedFuns.nonEmpty) {
      Console.println(i.module)
      val all = (i.privateFuns ++ i.exportedFuns).sorted
      val allStr = all.mkString(", ")
      Console.println(s"  $allStr")
    }

    Console.println("===")

    {
      // manual
      val privateCount = manual.map(_.privateFuns.size).sum
      val exportedCount = manual.map(_.exportedFuns.size).sum
      val totalCount = privateCount + exportedCount
      Console.println(s"""MANUAL:
           | private:  $privateCount
           | exported: $exportedCount
           | total:    $totalCount""".stripMargin)
    }

    {
      // generated
      val privateCount = gen.map(_.privateFuns.size).sum
      val exportedCount = gen.map(_.exportedFuns.size).sum
      val totalCount = privateCount + exportedCount
      Console.println(s"""GENERATED:
           | private:  $privateCount
           | exported: $exportedCount
           | total:    $totalCount""".stripMargin)
    }
  }

  private def analyze(): List[ModuleInfo] = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    apps.flatMap(analyzeApp)
  }

  def analyzeApp(app: App): List[ModuleInfo] =
    app.modules.map(analyzeModule)

  def analyzeModule(module: String): ModuleInfo = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    val listener = new NullaryUnspeccedListener()
    Pipeline.traverseForms(forms, listener)
    ModuleInfo(
      module,
      listener.isGenerated,
      listener.getPrivateFuns.toList.sorted,
      listener.getExportedFuns.toList.sorted,
    )
  }
}
