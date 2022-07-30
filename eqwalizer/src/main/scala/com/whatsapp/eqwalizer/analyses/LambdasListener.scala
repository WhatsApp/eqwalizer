/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Forms.FunDecl
import com.whatsapp.eqwalizer.ast.Pats.PatVar
import com.whatsapp.eqwalizer.ast.{AstListener, Lines}

import java.nio.file.{Files, Paths}

class LambdasListener extends AstListener {
  import LambdasListener._
  private val generatedMark: String = "@" + "generated"
  private var generatedMod: Boolean = false
  private var moduleToFile = Map[String, String]()
  private var lambdas = Set[LambdaInfo]()
  private var generatedLambdas = Set[LambdaInfo]()
  private var macroLineLambdas = Set[LambdaInfo]()
  private var fieldLambdas = List[LambdaInfo]()
  private var argLambdas = List[LambdaInfo]()
  private var callLambdas = List[LambdaInfo]()
  private var varLambdas = List[LambdaInfo]()
  private var varLambdasUsedMulti = Set[LambdaInfo]()
  private var tupleLambdas = List[LambdaInfo]()
  private var mapLambdas = List[LambdaInfo]()
  private var lastInCaseClauseLambdas = List[LambdaInfo]()
  private var lastInFunDeclLambdas = List[LambdaInfo]()
  private var currentModule: String = null
  private var lineBreaks: Array[Int] = null
  private var currentModuleLines: Vector[String] = null

  def analysis: Analysis = Analysis(
    moduleToFile,
    lambdas = lambdas,
    generatedLambdas = generatedLambdas,
    macroLineLambdas = macroLineLambdas,
    fieldLambdas = fieldLambdas,
    argLambdas = argLambdas,
    callLambdas = callLambdas,
    varLambdas = varLambdas,
    varLambdasUsedMulti = varLambdasUsedMulti,
    mapLambdas = mapLambdas,
    tupleLambdas = tupleLambdas,
    lastInCaseClauseLambdas = lastInCaseClauseLambdas,
    lastInFunDeclLambdas = lastInFunDeclLambdas,
  )

  override def enterModule(m: String, erlFile: String): Unit = {
    currentModule = m
    moduleToFile += m -> erlFile
    val bytes = Files.readAllBytes(Paths.get(erlFile))
    lineBreaks = Lines.toLineBreaks(bytes)
    val contents = new String(bytes)
    val preamble = contents.take(200)
    generatedMod = preamble.contains(generatedMark)
    currentModuleLines = contents.linesIterator.toVector
  }

  private var lambdaVars = Map[String, LambdaInfo]()
  private var usedLambdaVars = Set[String]()

  override def enterFunDecl(f: FunDecl): Unit = {
    lambdaVars = Map.empty
    usedLambdaVars = Set.empty
    f.clauses.map(_.body.exprs.last).foreach {
      case l: Lambda => lastInFunDeclLambdas ::= toLambdaInfo(l)
      case _         => ()
    }
  }

  override def enterExpr(e: Expr): Unit = e match {
    case Var(varName) if lambdaVars.contains(varName) =>
      if (usedLambdaVars(varName)) varLambdasUsedMulti += lambdaVars(varName)
      else usedLambdaVars += varName
    case Case(_, clauses) =>
      clauses.map(_.body.exprs.last).foreach {
        case l: Lambda => lastInCaseClauseLambdas ::= toLambdaInfo(l)
        case _         => ()
      }
    case l: Lambda =>
      val lambdaInfo = toLambdaInfo(l)
      lambdas += lambdaInfo
      if (generatedMod) generatedLambdas += lambdaInfo
      val line = Lines.asLine(l.pos, lineBreaks)
      if (currentModuleLines(line - 1).contains("?")) macroLineLambdas += lambdaInfo
    case DynCall(l: Lambda, _) =>
      callLambdas ::= toLambdaInfo(l)
    case Match(PatVar(varName), l: Lambda) =>
      val lambdaInfo = toLambdaInfo(l)
      lambdaVars += (varName -> lambdaInfo)
      varLambdas ::= lambdaInfo
    case LocalCall(_, args) =>
      args.foreach(handleArg)
    case RemoteCall(_, args) =>
      args.foreach(handleArg)
    case RecordCreate(_recName, fields) =>
      fields.map(_.value).foreach {
        case l: Lambda => fieldLambdas ::= toLambdaInfo(l)
        case _         => ()
      }
    case RecordUpdate(_expr, _recName, fields) =>
      fields.map(_.value).foreach {
        case l: Lambda => fieldLambdas ::= toLambdaInfo(l)
        case _         => ()
      }
    case MapCreate(kvs) =>
      kvs.foreach {
        case (_k, l: Lambda) => mapLambdas ::= toLambdaInfo(l)
        case _               => ()
      }
    case ReqMapUpdate(_, kvs) =>
      kvs.foreach {
        case (_k, l: Lambda) => mapLambdas ::= toLambdaInfo(l)
        case _               => ()
      }
    case GenMapUpdate(_, kvs) =>
      kvs.foreach {
        case (_k, l: Lambda) => mapLambdas ::= toLambdaInfo(l)
        case _               => ()
      }
    case Tuple(elems) =>
      elems.foreach {
        case l: Lambda => tupleLambdas ::= toLambdaInfo(l)
        case _         => ()
      }
    case _ => ()
  }

  private def toLambdaInfo(lambda: Lambda): LambdaInfo = LambdaInfo(
    module = currentModule,
    line = Lines.asLine(lambda.pos, lineBreaks),
    isNamed = lambda.name.isDefined,
  )

  private def handleArg(arg: Expr): Unit = arg match {
    case l: Lambda =>
      argLambdas ::= toLambdaInfo(l)
    case _ => ()
  }

}

object LambdasListener {
  case class LambdaInfo(module: String, line: Int, isNamed: Boolean)
  case class Analysis(
      moduleToFile: Map[String, String],
      lambdas: Set[LambdaInfo],
      generatedLambdas: Set[LambdaInfo],
      macroLineLambdas: Set[LambdaInfo],
      fieldLambdas: List[LambdaInfo],
      argLambdas: List[LambdaInfo],
      callLambdas: List[LambdaInfo],
      varLambdas: List[LambdaInfo],
      varLambdasUsedMulti: Set[LambdaInfo],
      tupleLambdas: List[LambdaInfo],
      mapLambdas: List[LambdaInfo],
      lastInCaseClauseLambdas: List[LambdaInfo],
      lastInFunDeclLambdas: List[LambdaInfo],
  )
}
