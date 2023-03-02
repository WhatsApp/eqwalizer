/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import java.nio.file.{Files, Paths}

import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.io.Project
import com.whatsapp.eqwalizer.tc.PipelineContext

class OverloadingUsesListener extends AstListener {

  case class FunctionInfo(
      location: String,
      generated: Boolean,
      clauseCount: Int,
      overlappingClauses: List[(Int, Int)],
  )

  private var currentModule: String = null
  private var currentFile: String = null
  private var infos: Map[RemoteId, FunctionInfo] = Map.empty
  private lazy val freshen = new TypeVars.VarFreshener().freshen _
  private var lineBreaks: Array[Int] = null
  private val generatedMark: String = "@" + "generated"
  private var generatedMod: Boolean = false

  def getInfos: Map[RemoteId, FunctionInfo] =
    infos

  override def enterModule(m: String, erlFile: String): Unit = {
    currentModule = m
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
    currentFile = Project.relativePath(erlFile)
    val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(200)
    generatedMod = preamble.contains(generatedMark)
  }

  override def exitModule(m: String): Unit =
    ()

  override def enterFunDecl(decl: Forms.FunDecl): Unit = {
    val ctx = PipelineContext(currentModule)
    val spec = DbApi.getOverloadedSpec(currentModule, decl.id)
    spec match {
      case Some(overloadedSpec) =>
        var useCount = Map.empty[Int, Int]
        overloadedSpec.tys.foreach { funTy =>
          val ft = freshen(funTy)
          val FunType(_, argTys, _) = ft
          useCount = decl.clauses.zipWithIndex.foldLeft(useCount)(countClauseUses(ctx, argTys))
        }
        val clauseCount = decl.clauses.size
        val overlappingClauses = (useCount.filter { case (_, uses) => uses > 1 }).toList
        val line = Lines.asLine(decl.pos, lineBreaks)
        val location = s"$currentFile:$line"
        val remoteId = RemoteId(currentModule, decl.id.name, decl.id.arity)
        val info = FunctionInfo(location, generatedMod, clauseCount, overlappingClauses)
        infos = infos + (remoteId -> info)
      case _ => ()
    }
  }

  private def countClauseUses(
      ctx: PipelineContext,
      argTys: List[Type],
  )(uses: Map[Int, Int], clauseInfo: (Exprs.Clause, Int)): Map[Int, Int] = {
    val (clause, clausePos) = clauseInfo
    val patVars = Vars.clausePatVars(clause)
    val env1 = ctx.util.enterScope(Map.empty, patVars)
    val env2 = ctx.elabGuard.elabGuards(clause.guards, env1)
    val (patTys, _) = ctx.elabPat.elabPats(clause.pats, argTys, env2)
    val reachable = !patTys.exists(ctx.subtype.isNoneType)
    if (reachable)
      return uses + (clausePos -> (uses.getOrElse(clausePos, 0) + 1))
    uses
  }
}
