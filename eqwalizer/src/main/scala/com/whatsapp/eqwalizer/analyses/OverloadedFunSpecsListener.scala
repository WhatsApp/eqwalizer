/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{AstListener, Forms, RemoteId}
import com.whatsapp.eqwalizer.io.Project

import scala.collection.mutable.ListBuffer
import com.whatsapp.eqwalizer.ast.Lines
import java.nio.file.{Files, Paths}

class OverloadedFunSpecsListener extends AstListener {
  var currentModule: String = null
  var currentFile: String = null
  val infos: ListBuffer[(String, RemoteId)] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getInfos: List[(String, RemoteId)] =
    infos.toList

  override def enterModule(m: String, erlFile: String): Unit = {
    currentModule = m
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
    currentFile = Project.relativePath(erlFile)
  }
  override def exitModule(m: String): Unit =
    ()

  override def enterFunSpec(spec: Forms.ExternalFunSpec): Unit =
    if (spec.types.size > 1) {

      val line = Lines.asLine(spec.pos, lineBreaks)
      val location = s"$currentFile:$line"
      val remoteId = RemoteId(currentModule, spec.id.name, spec.id.arity)
      infos.addOne((location, remoteId))
    }
}
