/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{AstListener, Forms, Lines, RemoteId}
import com.whatsapp.eqwalizer.io.Project

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

class OTPFunsListener extends AstListener {
  var currentFile: String = null
  val locations: ListBuffer[String] = ListBuffer.empty
  val infos: ListBuffer[(String, RemoteId)] = ListBuffer.empty
  private var lineBreaks: Array[Int] = null

  def getInfos: List[(String, RemoteId)] =
    infos.toList

  override def enterModule(m: String, erlFile: String): Unit =
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))

  override def enterFile(file: Forms.File): Unit =
    currentFile = Project.relativePath(file.file)

  override def enterExpr(e: Expr): Unit = e match {
    case fcall: RemoteCall =>
      val line = Lines.asLine(fcall.pos, lineBreaks)
      val location = s"$currentFile:$line"
      val module = fcall.id.module
      if (DbApi.otpModules(module))
        infos.addOne(location, fcall.id)
    case _ =>
  }
}
