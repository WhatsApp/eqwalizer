/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.io.Project

import scala.collection.mutable.ListBuffer
import java.nio.file.{Files, Paths}

class NegatedTypePredicatesListener extends AstListener {
  var currentFile: String = null
  var notLevel: Int = 0
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

  override def enterTest(test: Guards.Test): Unit = test match {
    case Guards.TestUnOp("not", _) =>
      notLevel += 1
    case Guards.TestCall(Id(predicate, _), _) =>
      // All type predicates (is_atom, is_binary, ...) start with `is_`
      // There is also `is_map_key` which starts with `is_`,
      // but which is not a true type predicate.
      if (predicate.startsWith("is_") && predicate != "is_map_key" && notLevel > 0) {
        val line = Lines.asLine(test.pos, lineBreaks)
        val location = s"$currentFile:$line"
        locations.addOne(location)
      }
    case _ =>
    // nothing
  }
  override def exitTest(test: Guards.Test): Unit = test match {
    case Guards.TestUnOp("not", _) =>
      notLevel -= 1
    case _ =>
    // nothing
  }
}
