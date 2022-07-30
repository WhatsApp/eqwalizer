/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses
import com.whatsapp.eqwalizer.ast.{AstListener, Forms}
import com.whatsapp.eqwalizer.ast.ExternalTypes._

import java.nio.file.{Files, Paths}

object NumbersListener {
  trait Stat {
    val name: String
    def generatedSpecCnt: Int
    def specCnt: Int
    def recCnt: Int
  }
  private trait StatInternal extends Stat {
    def matches(ty: ExtType): Boolean
    val name: String
    var found = false
    var generatedSpecCnt = 0
    var specCnt = 0
    var recCnt = 0
  }
  private class AllStat extends StatInternal {
    val name = "all"
    def matches(_ty: ExtType): Boolean = true
  }
  private class BuiltinStat(val name: String) extends StatInternal {
    def matches(ty: ExtType): Boolean = ty match {
      case BuiltinExtType(nme) if nme == name => true
      case _                                  => false
    }
  }
  private class RangeStat extends StatInternal {
    val name = "range"
    def matches(ty: ExtType): Boolean = ty match {
      case RangeExtType(_, _) => true
      case _                  => false
    }
  }
  private class IntLitStat extends StatInternal {
    val name = "int lit"
    def matches(ty: ExtType): Boolean = ty match {
      case IntLitExtType(_) => true
      case _                => false
    }
  }
}

class NumbersListener extends AstListener {
  import NumbersListener._
  private val generatedMark: String = "@" + "generated"
  private var generatedMod: Boolean = false
  private var currentModule: String = null

  private val stats: List[StatInternal] = List(
    new AllStat,
    new BuiltinStat("number"),
    new BuiltinStat("integer"),
    new BuiltinStat("pos_integer"),
    new BuiltinStat("non_neg_integer"),
    new BuiltinStat("neg_integer"),
    new BuiltinStat("byte"),
    new BuiltinStat("char"),
    new BuiltinStat("float"),
    new RangeStat,
    new IntLitStat,
  )

  def getTotal: Stat = stats.head
  def getStats: List[Stat] = stats.tail

  override def enterModule(m: String, erlFile: String): Unit = {
    currentModule = m
    val contents = new String(Files.readAllBytes(Paths.get(erlFile)))
    val preamble = contents.take(200)
    generatedMod = preamble.contains(generatedMark)
  }

  private def reset(): Unit = stats.foreach(_.found = false)

  override def enterFunSpec(spec: Forms.ExternalFunSpec): Unit = reset()

  override def exitFunSpec(spec: Forms.ExternalFunSpec): Unit = {
    stats.filter(_.found).foreach { stat =>
      stat.specCnt += 1
      if (generatedMod) stat.generatedSpecCnt += 1
    }
    reset()
  }

  override def enterRecDecl(recDecl: Forms.ExternalRecDecl): Unit = reset()

  override def exitRecDecl(recDecl: Forms.ExternalRecDecl): Unit = {
    stats.filter(_.found).foreach(stat => stat.recCnt += 1)
    reset()
  }

  override def enterType(ty: ExtType): Unit =
    stats.filter(_.matches(ty)).foreach(_.found = true)
}
