/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{AstListener, Exprs, Lines, Vars}
import com.whatsapp.eqwalizer.io.Project

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class PinnedVarsListener extends AstListener {
  private var lineBreaks: Array[Int] = null
  private val stack: ListBuffer[Either[Exprs.Clause, String]] = new ListBuffer()
  private val variables: mutable.HashMap[String, Int] = new mutable.HashMap()
  private val lambdaClauses: mutable.Set[Exprs.Clause] = mutable.Set()

  var currentFile: String = null
  val locations: ListBuffer[String] = ListBuffer.empty

  def getLocations: List[String] =
    locations.toList.sorted

  override def enterModule(m: String, erlFile: String): Unit = {
    currentFile = Project.relativePath(erlFile)
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
  }

  override def exitModule(m: String): Unit =
    currentFile = null

  override def enterExpr(e: Exprs.Expr): Unit = {
    e match {
      case Exprs.Match(pat, _) =>
        Vars.patVars(pat).foreach(add)
      case Exprs.Lambda(clauses) =>
        lambdaClauses.addAll(clauses)
      case _ =>
    }
  }

  override def exitExpr(e: Exprs.Expr): Unit = {
    e match {
      case Exprs.Lambda(clauses) =>
        lambdaClauses.subtractAll(clauses)
      case _ =>
    }
  }

  private def add(name: String): Unit = {
    Right(name) +=: stack
    val count = variables.getOrElseUpdate(name, 0)
    variables(name) = count + 1
  }

  private def check(clause: Exprs.Clause): Unit = {
    val vars = Vars.clausePatVars(clause)
    if (vars.exists(variables.contains)) {
      val line = Lines.asLine(clause.pos, lineBreaks)
      val location = s"$currentFile:$line"
      locations.addOne(location)
    }
  }

  override def enterClause(clause: Exprs.Clause): Unit = {
    // function clauses introduce fresh bindings/scopes
    if (!lambdaClauses(clause)) {
      check(clause)
    }
    Left(clause) +=: stack
    val vars = Vars.clausePatVars(clause)
    vars.foreach(add)
  }

  override def exitClause(clause: Exprs.Clause): Unit = {
    val vars = stack.takeWhile(_ != Left(clause))
    val names = vars.collect { case Right(name) =>
      name
    }
    names.foreach { name =>
      variables(name) = variables(name) - 1
      if (variables(name) == 0) {
        variables.remove(name)
      }
    }
    stack.dropWhileInPlace(_ != Left(clause))
    stack.dropInPlace(1)
  }
}
