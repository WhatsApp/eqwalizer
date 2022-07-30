/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.analyses.LambdasListener.LambdaInfo
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Forms.File
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.tc.PipelineContext

import java.nio.file.{Files, Paths}

class CheckableFunsListener extends AstListener {
  import CheckableFunsListener._

  // CheckableFunsListener is not a generated file in the first place,
  // working around how generated file are recognised.
  private val generatedMark: String = "@" + "generated"
  private def currentModule: String = pCtx.module
  private var generatedMod: Boolean =
    false
  private var testMod: Boolean = false
  private var currentFun: Id =
    null
  // functions referenced in the current fun
  private var usedFuns: Set[Use] =
    Set()
  private var usedLambdas: Set[Lambda] =
    Set()
  private var checkableFuns: Set[RemoteId] =
    Set()
  private var nonGenFuns: Set[RemoteId] = Set()
  private var nonTestFuns: Set[RemoteId] = Set()
  private var speccedFuns: Set[RemoteId] = Set()
  private var funs: Set[RemoteId] = Set()
  private var pCtx: PipelineContext = null
  private var lineBreaks: Array[Int] = null

  // set from outside for each module
  var lambdasAnalysis: LambdasListener.Analysis = null

  def analysis: Analysis = Analysis(
    funs = funs,
    checkable = checkableFuns,
    nonGenerated = nonGenFuns,
    specced = speccedFuns,
    nonTest = nonTestFuns,
  )

  override def enterFile(file: File): Unit = testMod = file.file.contains("/test/")

  override def enterModule(m: String, erlFile: String): Unit = {
    pCtx = PipelineContext(m)
    require(lambdasAnalysis != null)
    lineBreaks = Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
    val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(200)
    generatedMod = preamble.contains(generatedMark)
  }
  override def exitModule(m: String): Unit = {
    pCtx = null
    lambdasAnalysis = null
  }

  override def enterExpr(expr: Expr): Unit = expr match {
    case LocalCall(id, _) =>
      usedFuns += IdUse(id, true)
    case RemoteCall(rid, _) =>
      usedFuns += RemoteIdUse(rid, true)
    case DynCall(Lambda(_), _) =>
      ()
    case l: Lambda =>
      usedLambdas += l
    case LocalFun(id) =>
      usedFuns += IdUse(id, false)
    case RemoteFun(rid) =>
      usedFuns += RemoteIdUse(rid, false)
    case _ =>
    // nothing
  }

  override def exitExpr(e: Expr): Unit = ()

  override def enterFunDecl(funDecl: Forms.FunDecl): Unit = {
    currentFun = funDecl.id
    if (pCtx.gradualTyping) {
      val remoteId = pCtx.util.globalFunId(currentModule, funDecl.id)
      speccedFuns += remoteId
    }
  }

  override def enterFunSpec(spec: Forms.ExternalFunSpec): Unit = {
    val remoteId = pCtx.util.globalFunId(currentModule, spec.id)
    speccedFuns += remoteId
  }

  override def exitFunDecl(funDecl: Forms.FunDecl): Unit = {
    assessFun()
    currentFun = null
    usedFuns = Set()
    usedLambdas = Set()
  }

  private def isTyped(use: Use): Boolean = {
    val (remoteId, appPosition) =
      use match {
        case IdUse(id, appPosition) =>
          val remoteId = pCtx.util.globalFunId(currentModule, id)
          (remoteId, appPosition)
        case RemoteIdUse(remoteId, appPosition) =>
          (remoteId, appPosition)
      }
    val isAppCustom = appPosition && pCtx.elabApplyCustom.isCustom(remoteId)
    val isAppOverloadedFun = appPosition && pCtx.elabApplyOverloaded.isOverloadedFun(remoteId)
    val isSpecced = pCtx.util.getFunType(remoteId).isDefined
    isAppCustom || isAppOverloadedFun || isSpecced
  }

  private def assessFun(): Unit = {
    val funId = RemoteId(currentModule, currentFun.name, currentFun.arity)
    funs += funId
    val hasSpec = pCtx.util.getFunType(funId).isDefined || pCtx.util.getOverloadedSpec(funId).isDefined
    val allLambdasInCheckPosition = usedLambdas.forall(isCheckedLambda)
    val allFunsAreSpecced = usedFuns.forall(isTyped)
    val checkableFun = hasSpec && allLambdasInCheckPosition && allFunsAreSpecced
    if (!generatedMod) nonGenFuns += funId
    if (!testMod) nonTestFuns += funId
    if (checkableFun) checkableFuns += funId
  }

  /** lambda will be handled by Check.scala (analysis mode) rather than Elab.scala (synthesis mode)
    * *or* are immediately called: `(fun (x) -> x + 1)(3)`
    */
  private def isCheckedLambda(lambda: Lambda): Boolean = {
    if (pCtx.gradualTyping)
      return true
    val a = lambdasAnalysis
    val line = Lines.asLine(lambda.pos, lineBreaks)
    val lambdaInfo = LambdaInfo(currentModule, line = line, lambda.name.isDefined)
    lazy val isZeroArity = lambda.clauses.head.pats.isEmpty
    lazy val isDirectArg = a.argLambdas.contains(lambdaInfo)
    lazy val isImmediatelyCalled = a.callLambdas.contains(lambdaInfo)
    // underapproximation
    lazy val isCheckMode = a.fieldLambdas.contains(lambdaInfo) || a.lastInFunDeclLambdas.contains(lambdaInfo)
    isZeroArity || isDirectArg || isCheckMode || isImmediatelyCalled
  }
}

object CheckableFunsListener {
  case class Analysis(
      funs: Set[RemoteId],
      checkable: Set[RemoteId],
      nonGenerated: Set[RemoteId],
      specced: Set[RemoteId],
      nonTest: Set[RemoteId],
  )
  sealed trait Use
  case class IdUse(id: Id, appPosition: Boolean) extends Use
  case class RemoteIdUse(remoteId: RemoteId, appPosition: Boolean) extends Use
}
