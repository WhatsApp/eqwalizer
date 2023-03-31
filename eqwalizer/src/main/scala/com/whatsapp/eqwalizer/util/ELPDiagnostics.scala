/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import scala.collection.mutable

import com.whatsapp.eqwalizer.{Pipeline, ast}
import com.whatsapp.eqwalizer.ast.Forms.{ElpMetadata, FuncDecl, InternalForm, InvalidForm, MisBehaviour}
import com.whatsapp.eqwalizer.ast.{Pos, Show, TextRange}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.io.Ipc
import com.whatsapp.eqwalizer.tc.{Options, noOptions}

object ELPDiagnostics {
  case class Error(
      position: Pos,
      message: String,
      uri: String,
      errorName: String,
      explanation: Option[String],
      shownExpression: Option[String],
  )

  def getDiagnosticsString(module: String, astStorage: DbApi.AstStorage, options: Options = noOptions): String =
    toJsonObj(Map(module -> getDiagnostics(module, astStorage, options))).render(indent = 2)

  def getDiagnosticsIpc(modulesAndStorages: Iterable[(String, DbApi.AstStorage)]): Unit =
    try {
      val diagnosticsByModule = mutable.Map.empty[String, List[Error]]
      for { (module, astStorage) <- modulesAndStorages } {
        Ipc.sendEqwalizingStart(module)
        val diagnostics = getDiagnostics(module, astStorage, noOptions)
        Ipc.sendEqwalizingDone(module)
        if (diagnostics.nonEmpty) {
          diagnosticsByModule.put(module, diagnostics)
        }
      }
      Ipc.sendDone(diagnosticsByModule)
    } catch {
      case Ipc.Terminated => ()
    }

  private def getDiagnostics(module: String, astStorage: DbApi.AstStorage, options: Options): List[Error] = {
    val invalidForms = DbApi.getInvalidForms(module).get
    val forms = Pipeline.checkForms(astStorage, options) ++ invalidForms
    formsToErrors(module, forms).sortBy(_.position.productElement(0).asInstanceOf[Int])
  }

  private def formsToErrors(module: String, forms: List[InternalForm]): List[Error] = {
    val elpMetadata = forms.collectFirst { case elpMetadata: ElpMetadata =>
      elpMetadata
    }
    val (forms1, redundantFixmes) = Pipeline.applyFixmes(forms, elpMetadata)
    val errors = forms1.collect {
      case ef: InvalidForm =>
        List(ef.te)
      case MisBehaviour(te) =>
        List(te)
      case FuncDecl(_, errors) =>
        errors
    }.flatten ++ redundantFixmes
    errors.map { te =>
      Error(
        te.pos,
        te.msg,
        te.docURL,
        te.errorName,
        explanation = te.explanation,
        shownExpression = te.erroneousExpr.map(Show.show),
      )
    }
  }

  def toJsonObj(errorsByModule: collection.Map[String, List[Error]]): ujson.Obj = {
    ujson.Obj.from(errorsByModule.map { case (module, errors) =>
      module -> ujson.Arr.from(errors.map { e =>
        val (range, lineAndCol) = e.position match {
          case TextRange(startByte, endByte) =>
            val range = ujson.Obj("start" -> startByte, "end" -> endByte)
            val lineAndCol = ujson.Null
            (range, lineAndCol)
          case ast.LineAndColumn(line, col) =>
            val bs = ujson.Null
            val lineAndCol = ujson.Obj("line" -> line, "col" -> col)
            (bs, lineAndCol)
        }
        val expressionOrNull = e.shownExpression match {
          case Some(s) => ujson.Str(s)
          case None    => ujson.Null
        }
        val explanationOrNull = e.explanation match {
          case Some(s) => ujson.Str(s)
          case None    => ujson.Null
        }
        ujson.Obj(
          "range" -> range,
          "lineAndCol" -> lineAndCol,
          "message" -> ujson.Str(e.message),
          "uri" -> ujson.Str(e.uri),
          "code" -> ujson.Str(e.errorName),
          "expressionOrNull" -> expressionOrNull,
          "explanationOrNull" -> explanationOrNull,
        )
      })
    })
  }
}
