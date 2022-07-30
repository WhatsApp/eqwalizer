/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.{AstListener, Forms}
import com.whatsapp.eqwalizer.io.Project

import scala.collection.mutable.ListBuffer
import com.whatsapp.eqwalizer.ast.Lines
import java.nio.file.{Files, Paths}

class ApproximatedMapsListener extends AstListener {
  var currentFile: String = null
  val locations: ListBuffer[String] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getLocations: List[String] =
    locations.toList

  override def enterModule(m: String, erlFile: String): Unit =
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))

  override def enterFile(file: Forms.File): Unit =
    currentFile = Project.relativePath(file.file)

  override def traverseHeaders(): Boolean = false

  override def enterExpr(e: Expr): Unit = e match {
    case gu: GenMapUpdate if gu.approximated =>
      val line = Lines.asLine(gu.pos, lineBreaks)
      val location = s"$currentFile:$line"
      locations.addOne(location)
    case _ =>
  }

  override def enterTest(t: Test): Unit = t match {
    case gu: TestGenMapUpdate if gu.approximated =>
      val line = Lines.asLine(gu.pos, lineBreaks)
      val location = s"$currentFile:$line"
      locations.addOne(location)
    case _ =>
  }
}
