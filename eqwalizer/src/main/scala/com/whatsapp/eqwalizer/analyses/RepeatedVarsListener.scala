/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{AstListener, Exprs, Lines}
import com.whatsapp.eqwalizer.io.Project
import com.whatsapp.eqwalizer.tc.PipelineContext

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

class RepeatedVarsListener extends AstListener {
  private var lineBreaks: Array[Int] = null

  var ctx: PipelineContext = null
  var currentFile: String = null
  val locations: ListBuffer[String] = ListBuffer.empty

  def getLocations: List[String] =
    locations.toList.sorted

  override def enterModule(m: String, erlFile: String): Unit = {
    ctx = PipelineContext(m)
    currentFile = Project.relativePath(erlFile)
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
  }

  override def exitModule(m: String): Unit =
    currentFile = null

  override def exitClause(clause: Exprs.Clause): Unit = {
    val vars = ctx.vars.clausePatVarsL(clause)
    val hasRepeatedVars = vars.toSet.size != vars.size
    if (hasRepeatedVars) {
      val line = Lines.asLine(clause.pos, lineBreaks)
      val location = s"$currentFile:$line"
      locations.addOne(location)
    }
  }
}
