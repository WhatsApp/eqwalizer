/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.AstListener
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.io.Project

import scala.collection.mutable.ListBuffer
import com.whatsapp.eqwalizer.ast.Lines
import java.nio.file.{Files, Paths}

class TryOfCatchListener extends AstListener {
  var currentFile: String = null
  val locations: ListBuffer[String] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getLocations: List[String] =
    locations.toList.sorted

  override def enterModule(m: String, erlFile: String): Unit = {
    currentFile = Project.relativePath(erlFile)
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
  }

  override def exitModule(m: String): Unit =
    ()

  override def exitExpr(e: Expr): Unit =
    e match {
      case tryOf: TryOfCatchExpr =>
        val line = Lines.asLine(tryOf.pos, lineBreaks)
        val location = s"$currentFile:$line"
        locations.addOne(location)
      case _ =>
    }
}
