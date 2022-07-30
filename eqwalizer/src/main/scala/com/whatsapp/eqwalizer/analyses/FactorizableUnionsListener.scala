/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.io.Project

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

// $COVERAGE-OFF$
class FactorizableUnionsListener extends AstListener {

  private var currentModule: String = null
  private var currentFile: String = null
  private val results: ListBuffer[(String, Int, UnionExtType)] = ListBuffer[(String, Int, UnionExtType)]()
  private var lineBreaks: Array[Int] = null
  private val generatedMark: String = "@" + "generated"
  private var generatedMod: Boolean = false

  def getResults: List[(String, Int, UnionExtType)] =
    results.toList

  override def enterModule(m: String, erlFile: String): Unit = {
    currentModule = m
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
    currentFile = Project.relativePath(erlFile)
    val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(200)
    generatedMod = preamble.contains(generatedMark)
  }

  override def exitModule(m: String): Unit =
    ()

  def diffCoord(tys1: List[ExtType], tys2: List[ExtType]): Option[Int] = {
    if (tys1.size != tys2.size) {
      None
    } else {
      val diffs = tys1.zip(tys2).zipWithIndex.filterNot(p => p._1._1 == p._1._2)
      if (diffs.size != 1) None
      else diffs.headOption.map(_._2)
    }
  }

  override def enterType(tp: ExtType): Unit =
    tp match {
      case union: UnionExtType =>
        if (!generatedMod) {
          val tuples = union.tys.collect { case t: TupleExtType => t }.zipWithIndex
          var coords = Set.empty[Int]
          tuples.foreach(t1 =>
            tuples.foreach(t2 =>
              if (t1._2 < t2._2) {
                diffCoord(t1._1.argTys, t2._1.argTys) match {
                  case None    => ()
                  case Some(i) => coords += i
                }
              }
            )
          )
          if (coords.nonEmpty) {
            val line = Lines.asLine(union.pos, lineBreaks)
            val location = s"$currentFile:$line"
            results.addOne(location, coords.size, union)
          }
        }
      case _ =>
    }
}
