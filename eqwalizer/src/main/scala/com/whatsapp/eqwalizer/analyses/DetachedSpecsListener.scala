/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.io.Project

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

object DetachedSpecsListener {
  case class Location(source: String, line: Int, id: Id)
}
class DetachedSpecsListener extends AstListener {
  import DetachedSpecsListener.Location
  private var currentFile: Option[String] = None
  private var lastSpec: Option[(Id, Pos)] = None
  private val locations: ListBuffer[Location] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getLocations: List[Location] =
    locations.toList.sortBy(_.line)

  override def enterFunDecl(funDecl: Forms.FunDecl): Unit = {
    for ((id, pos) <- lastSpec if id != funDecl.id) {
      val Some(source) = currentFile
      val line = Lines.asLine(pos, lineBreaks)
      locations.addOne(Location(source, line, id))
    }
  }
  override def exitFunDecl(funDecl: Forms.FunDecl): Unit = {
    lastSpec = None
  }

  override def enterFunSpec(spec: Forms.ExternalFunSpec): Unit = {
    for ((id, pos) <- lastSpec) {
      val Some(source) = currentFile
      val line = Lines.asLine(pos, lineBreaks)
      locations.addOne(Location(source, line, id))
    }
  }
  override def exitFunSpec(spec: Forms.ExternalFunSpec): Unit = {
    lastSpec = Some((spec.id, spec.pos))
  }

  override def enterModule(m: String, erlFile: String): Unit = {
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
    currentFile = Some(Project.relativePath(erlFile))
  }

  override def exitModule(m: String): Unit = {
    // it may be the last spec in the file!
    for ((id, pos) <- lastSpec) {
      val Some(source) = currentFile
      val line = Lines.asLine(pos, lineBreaks)
      locations.addOne(Location(source, line, id))
    }
    currentFile = None
    lastSpec = None
  }
}
