/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.{Pipeline, ast}
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.{Pos, Show, TextRange}
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.io.Ipc
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{RedundantFixme, TypeError}
import com.whatsapp.eqwalizer.tc.{Options, noOptions}
import com.github.plokhotnyuk.jsoniter_scala.core._

object ELPDiagnostics {
  case class Error(
      position: Pos,
      message: String,
      uri: String,
      errorName: String,
      explanation: Option[String],
      shownExpression: Option[String],
      diagnostic: Diagnostic.Diagnostic,
  )

  def getDiagnosticsIpc(modules: Iterable[String]): Unit =
    try {
      for { module <- modules } {
        if (Ipc.shouldEqwalize(module)) {
          Ipc.sendEqwalizingStart(module)
          val diagnostics = getDiagnostics(module, noOptions)
          Ipc.sendEqwalizingDone(module)
          Ipc.finishEqwalization(Map(module -> diagnostics), Db.getLoadedModules().toList)
        }
      }
      Ipc.sendDone(Map.empty)
    } catch {
      case Ipc.Terminated => Ipc.sendDone(Map.empty)
    }

  private def getDiagnostics(module: String, options: Options): List[Error] = {
    val (typeErrors, invalids, redundantFixmes) = Pipeline.checkForms(module, options)
    toELPErrors(typeErrors, invalids, redundantFixmes).sortBy(_.position.productElement(0).asInstanceOf[Int])
  }

  private def toELPErrors(
      errors: List[TypeError],
      invalids: List[Invalid],
      redundantFixmes: List[RedundantFixme],
  ): List[Error] =
    (errors ++ invalids ++ redundantFixmes).map { te =>
      Error(
        te.pos,
        te.msg,
        te.docURL,
        te.errorName,
        explanation = te.explanation,
        shownExpression = te.erroneousExpr.map(Show.show),
        diagnostic = te,
      )
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
        val diagnosticJson = e.diagnostic match {
          case te: TypeError => ujson.Obj("TypeError" -> ujson.read(writeToString(te)))
          case inv: Invalid  => ujson.Obj("InvalidForm" -> ujson.read(writeToString(inv)))
        }
        ujson.Obj(
          "range" -> range,
          "lineAndCol" -> lineAndCol,
          "message" -> ujson.Str(e.message),
          "uri" -> ujson.Str(e.uri),
          "code" -> ujson.Str(e.errorName),
          "expressionOrNull" -> expressionOrNull,
          "explanationOrNull" -> explanationOrNull,
          "diagnostic" -> diagnosticJson,
        )
      })
    })
  }
}
