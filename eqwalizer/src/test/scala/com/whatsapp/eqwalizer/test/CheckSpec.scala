/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.config
import com.whatsapp.eqwalizer.tc.{Options, noOptions}
import com.whatsapp.eqwalizer.util.{ELPDiagnostics, TcDiagnosticsText}

class CheckSpec extends SnapshotSpec {

  testDir(srcDir = "test_projects/check/src")
  testDir(srcDir = "test_projects/check/test")
  testDir(srcDir = "test_projects/check_gradual/src", options = Options(gradualTyping = Some(true)))
  testDir(srcDir = "test_projects/debug/src")
  testDir(srcDir = "test_projects/elm_core/src", codeWidth = 80)
  testDir(srcDir = "test_projects/eqwater/src")
  testDir(srcDir = "test_projects/options/src", options = Options(checkRedundantGuards = Some(true)))
  testDir(
    srcDir = "test_projects/fault_tolerance/src",
    options = Options(gradualTyping = Some(true), eqwater = Some(true), tolerateErrors = Some(true)),
  )
  testDir(
    srcDir = "test_projects/fault_tolerance/src",
    options = Options(gradualTyping = Some(true), eqwater = Some(true), tolerateErrors = Some(true)),
    json = true,
  )

  def testDir(
      srcDir: String,
      codeWidth: Int = config.codeWidth,
      options: Options = noOptions,
      json: Boolean = false,
  ): Unit = {
    describe(srcDir) {
      val srcPath = Paths.get(srcDir)
      val erlPaths = Files
        .find(srcPath, 10, (path, attrs) => attrs.isRegularFile && path.getFileName.toString.endsWith(".erl"))
        .iterator()
        .asScala
        .toList
      val modules = erlPaths.map(_.getFileName.toString.dropRight(4))
      modules.foreach(testModule(srcDir, _, codeWidth, options, json))
    }
  }

  def testModule(srdDir: String, module: String, width: Int, options: Options, json: Boolean): Unit = {
    val formatSuffix = if (json) " (json)" else ""
    it(s"$srdDir/$module.erl$formatSuffix") {
      val astStorage = DbApi.getAstStorage(module).get
      val (expPath, diag) =
        if (json)
          (Paths.get(s"$srdDir/$module.erl.json"), ELPDiagnostics.getDiagnosticsString(module, astStorage, options))
        else
          (
            Paths.get(s"$srdDir/$module.erl.check"),
            TcDiagnosticsText(width, lineNumbers = false).checkFile(astStorage, options).mkString("", "\n", "\n"),
          )
      if (generateOut) Files.write(expPath, diag.getBytes)
      val exp = new String(Files.readAllBytes(expPath))
      assert(exp == diag)
    }
  }
}
