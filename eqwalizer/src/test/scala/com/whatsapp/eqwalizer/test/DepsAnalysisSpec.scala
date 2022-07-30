/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.analyses.{ModuleFunDeps, ModuleTypeDeps}
import com.whatsapp.eqwalizer.ast.{Id, RemoteId}

import java.nio.file.{Files, Paths}

class DepsAnalysisSpec extends SnapshotSpec {
  testDir(srcDir = "test_projects/misc/src")

  def testDir(srcDir: String): Unit = {
    describe(srcDir) {
      val files = Paths.get(srcDir).toFile.listFiles().sorted
      val erlFiles = files.filter(f => f.isFile && f.getPath.endsWith(".erl"))
      val modules = erlFiles.map(_.getName).map(_.dropRight(".erl".length))
      modules.foreach(testModuleFunDeps(srcDir, _))
      modules.foreach(testModuleTypeDeps(srcDir, _))
    }
  }

  private def testModuleFunDeps(srcDir: String, module: String): Unit =
    it(s"funDeps: $srcDir/$module.erl") {
      val funDeps = ModuleFunDeps.getFunDeps(module)
      val funDepsJsonTxt = depsJson(funDeps).render(indent = 2)
      val expPath = Paths.get(s"$srcDir/$module.fun_deps.json")
      if (generateOut) Files.write(expPath, funDepsJsonTxt.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp === funDepsJsonTxt)
    }

  private def testModuleTypeDeps(srcDir: String, module: String): Unit =
    it(s"typeDeps: $srcDir/$module.erl") {
      val funDeps = ModuleTypeDeps.getTypeDeps(module)
      val funDepsJsonTxt = depsJson(funDeps).render(indent = 2)
      val expPath = Paths.get(s"$srcDir/$module.type_deps.json")
      if (generateOut) Files.write(expPath, funDepsJsonTxt.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp === funDepsJsonTxt)
    }

  private def depsJson(funDeps: Map[RemoteId, List[RemoteId]]): ujson.Obj = {
    val items = funDeps
      .map { case (funId, rids) =>
        val key = funId.toString
        val value: ujson.Value =
          ujson.Arr.from(rids.map(_.toString).map(ujson.Str))
        (key, value)
      }
      .toList
      .sortBy(_._1)
    ujson.Obj.from(items)
  }
}
