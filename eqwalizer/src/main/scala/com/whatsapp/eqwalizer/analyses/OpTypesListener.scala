/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.{AstListener, Forms, Lines}
import com.whatsapp.eqwalizer.io.Project

import scala.collection.mutable.ListBuffer
import java.nio.file.{Paths, Files}

class OpTypesListener extends AstListener {
  var currentFile: String = null
  var notLevel: Int = 0
  val infos: ListBuffer[(String, String)] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getInfos: List[(String, String)] =
    infos.toList

  override def enterModule(m: String, erlFile: String): Unit =
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))

  override def enterFile(file: Forms.File): Unit =
    currentFile = Project.relativePath(file.file)

  override def enterType(tp: ExtType): Unit =
    tp match {
      case unOp: UnOpType =>
        val op = unOp.op
        val line = Lines.asLine(unOp.pos, lineBreaks)
        val location = s"$currentFile:$line"
        infos.addOne((op, location))
      case binOp: BinOpType =>
        val op = binOp.op
        val line = Lines.asLine(binOp.pos, lineBreaks)
        val location = s"$currentFile:$line"
        infos.addOne((op, location))
      case _ =>
    }
}
