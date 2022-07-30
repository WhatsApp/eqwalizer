/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Paths}

abstract class SnapshotSpec extends AnyFunSpec with BeforeAndAfterAllConfigMap {
  protected var generateOut: Boolean = false

  override protected def beforeAll(configMap: ConfigMap): Unit =
    generateOut = configMap.get("generate_out").isDefined

  def checkActionFile(action: => Unit, actualFile: String, expFile: String): Unit = {
    action
    val outPath = Paths.get(actualFile)
    val expPath = Paths.get(s"test_projects/_cli/$expFile")
    val outText = new String(Files.readAllBytes(outPath))
    if (generateOut) Files.write(expPath, outText.getBytes())
    val expOutTxt = new String(Files.readAllBytes(expPath))
    assert(expOutTxt === outText)
  }

  def checkAction(action: => Unit, expOutPath: String): Unit = {
    val out = new ByteArrayOutputStream
    Console.withOut(out)(action)
    val outText = out.toString

    val expPath = Paths.get(s"test_projects/_cli/$expOutPath")
    if (generateOut) Files.write(expPath, outText.getBytes)
    val expText = new String(Files.readAllBytes(expPath))

    assert(outText === expText)
  }

  def checkJsonAction(action: => Unit, expOutPath: String, transform: ujson.Value => ujson.Value): Unit = {
    val out = new ByteArrayOutputStream
    Console.withOut(out)(action)
    val outVal = transform(ujson.read(out.toString))
    val outText = outVal.render(indent = 2)

    val expPath = Paths.get(s"test_projects/_cli/$expOutPath")
    if (generateOut) Files.write(expPath, outText.getBytes)
    val expText = new String(Files.readAllBytes(expPath))

    assert(outText === expText)
  }
}
