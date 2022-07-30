/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

// $COVERAGE-OFF$
package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.{File, TypingAttribute}
import com.whatsapp.eqwalizer.ast.{App, Forms}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.io.Project

import java.nio.file.{Files, Paths}

object EqwalizedModules {
  private val generatedMark: String = "@" + "generated"

  case class ModuleInfo(module: String, path: String, generated: Boolean, production: Boolean, eqwalized: Boolean)
  def main(args: Array[String]): Unit = {
    val data = analyze()
    val modulePaths = data.map(_.path).toList.sorted
    val prodPaths = data.filter(_.production).map(_.path).toList.sorted
    val testPaths = data.filter(mi => !mi.production).map(_.path).toList.sorted
    val prodNonGenPaths = data.filter(mi => mi.production && !mi.generated).map(_.path).toList.sorted
    val eqwalizedProdNonGenPaths =
      data.filter(mi => mi.production && !mi.generated && mi.eqwalized).map(_.path).toList.sorted

    println(s"*** ALL MODULES ${modulePaths.size} ***")
    modulePaths.foreach(println)
    println()

    println(s"*** PROD MODULES ${prodPaths.size} ***")
    prodPaths.foreach(println)
    println()

    println(s"*** TEST MODULES ${testPaths.size} ***")
    testPaths.foreach(println)
    println()

    println(s"*** PROD NON-GEN MODULES ${prodNonGenPaths.size} ***")
    prodNonGenPaths.foreach(println)
    println()

    println(s"*** EQWALIZED PROD NON-GEN MODULES ${eqwalizedProdNonGenPaths.size} ***")
    eqwalizedProdNonGenPaths.foreach(println)
    println()

    val ratio = (eqwalizedProdNonGenPaths.size.toFloat * 100 / prodNonGenPaths.size.toFloat).toInt
    println(s"*** RATIO: $ratio % ***")
  }

  private def analyze(): Iterable[ModuleInfo] =
    analyzeApps(DbApi.projectApps.values.toList)

  private def analyzeApps(apps: List[App]): Iterable[ModuleInfo] =
    apps.flatMap(analyzeApp)

  private def analyzeApp(app: App): Iterable[ModuleInfo] =
    app.modules.map(analyzeModule)

  private def analyzeModule(module: String): ModuleInfo = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    val absPath = forms.collectFirst { case File(path, _) => path }.get
    val path = Project.relativePath(absPath)
    val production = !absPath.contains("/test/")
    val preamble = new String(Files.readAllBytes(Paths.get(absPath))).take(200)
    val generated = preamble.contains(generatedMark)
    val eqwalized = forms.exists { case TypingAttribute("eqwalizer" :: _) => true; case _ => false }
    ModuleInfo(module = module, path = path, generated = generated, production = production, eqwalized = eqwalized)
  }
}
