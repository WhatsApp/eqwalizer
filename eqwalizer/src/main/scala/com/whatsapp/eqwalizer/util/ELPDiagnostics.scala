/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.{Pipeline, ast}
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.{InvalidDiagnostics, Pos, Show, TextRange}
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.io.Ipc
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{RedundantFixme, TypeError}
import com.whatsapp.eqwalizer.tc.{Options, TcDiagnostics, TypeInfo, noOptions}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic

object ELPDiagnostics {
  case class Error(
      range: Option[TextRange],
      message: String,
      uri: String,
      code: String,
      explanation: Option[String],
      expression: Option[String],
      diagnostic: StructuredDiagnostic,
  )

  sealed trait StructuredDiagnostic
  case class TypeError(error: TcDiagnostics.TypeError) extends StructuredDiagnostic
  case class InvalidForm(invalid: InvalidDiagnostics.Invalid) extends StructuredDiagnostic

  implicit val codec: JsonValueCodec[Map[String, List[Error]]] = JsonCodecMaker.make(
    CodecMakerConfig
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  def getDiagnosticsIpc(modules: Iterable[String]): Unit =
    try {
      for { module <- modules } {
        if (Ipc.shouldEqwalize(module)) {
          Ipc.sendEqwalizingStart(module)
          val diagnostics = getDiagnostics(module, noOptions)
          Ipc.sendEqwalizingDone(module)
          Ipc.finishEqwalization(Map(module -> diagnostics), Db.getLoadedModules().toList, TypeInfo.toIpc())
        }
      }
      Ipc.sendDone(Map.empty, Map.empty)
    } catch {
      case Ipc.Terminated => Ipc.sendDone(Map.empty, Map.empty)
    }

  private def getDiagnostics(module: String, options: Options): List[Error] = {
    val (typeErrors, invalids, redundantFixmes) = Pipeline.checkForms(module, options)
    toELPErrors(typeErrors, invalids, redundantFixmes).sortBy(_.range.map(_.startByte))
  }

  private def toELPErrors(
      errors: List[Diagnostic],
      invalids: List[Invalid],
      redundantFixmes: List[RedundantFixme],
  ): List[Error] =
    (errors ++ invalids ++ redundantFixmes).map { te =>
      Error(
        te.pos match { case tr: TextRange => Some(tr); case _ => None },
        te.msg,
        te.docURL,
        te.errorName,
        explanation = te.explanation,
        expression = te.erroneousExpr.map(Show.show),
        diagnostic = te match {
          case te: TcDiagnostics.TypeError => TypeError(te)
          case invalid: Invalid            => InvalidForm(invalid)
        },
      )
    }
}
