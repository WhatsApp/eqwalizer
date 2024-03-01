/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import java.nio.file.{Files, Path, Paths}
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{Lines, Show}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.stub.DbApi.AstStorage
import com.whatsapp.eqwalizer.tc.{Options, noOptions}
import com.whatsapp.eqwalizer.{Pipeline, config}

case class TcDiagnosticsText(width: Int = config.codeWidth, lineNumbers: Boolean = true) {
  private implicit class TextOps(text: String) {
    def colTrim(width: Int, lastColumn: Boolean = false): String =
      if (text.length <= width) {
        if (lastColumn) text else text.padTo(width, ' ')
      } else {
        text.take(width - 2) ++ "……"
      }
  }

  sealed trait Status {
    def msg: String
  }
  object NoSpecStatus extends Status {
    def msg: String = "NO SPEC"
  }
  object OkStatus extends Status {
    def msg: String = "OK"
  }
  object ErrorStatus extends Status {
    def msg: String = "ERROR"
  }
  object InvalidStatus extends Status {
    def msg: String = "INVALID"
  }

  private case class DLine(
      line: Int,
      text: String,
      status: Option[Status],
      errors: List[Diagnostic.Diagnostic],
  ) {
    def format(): String = {
      val lineNum =
        if (lineNumbers)
          line.toString.reverse.padTo(4, ' ').reverse ++ " "
        else
          ""
      val lineText =
        if (text.length <= width) text.padTo(width, ' ') ++ " |"
        else text.take(width) ++ "……"
      val diagText = status.map(_.msg).getOrElse("").colTrim(7)
      def msg(err: Diagnostic.Diagnostic): String = {
        val erroneousTxt = err.erroneousExpr match {
          case Some(expr) => s"${Show.show(expr)}.\n"
          case None       => ""
        }
        val explanationTxt = err.explanation match {
          case Some(explanation) => s"\n\n$explanation"
          case None              => ""
        }
        s"$erroneousTxt${err.msg}$explanationTxt"
      }
      val errorTexts = errors.map(msg).mkString("\n---\n").split('\n')
      val tailPrefix =
        if (lineNumbers)
          "".padTo(width + 6, ' ') ++ "|         | "
        else
          "".padTo(width + 1, ' ') ++ "|         | "
      val tailText = errorTexts.tail.map(tailPrefix ++ _).mkString("\n")
      val (head, tail) =
        if (errors.isEmpty) ("", "")
        else {
          val exprStr = errors.head.erroneousExpr match {
            case Some(expr) => s"${Show.show(expr)}.\n"
            case None       => ""
          }
          (" " ++ errorTexts.head, "\n" ++ tailText)
        }
      s"$lineNum$lineText $diagText |$head$tail"
    }
  }

  def checkFile(astStorage: DbApi.AstStorage, options: Options = noOptions): List[String] =
    checkFileD(astStorage, options).map(_.format())

  private def checkFileD(astStorage: DbApi.AstStorage, options: Options): List[DLine] = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val forms = Pipeline.checkForms(astStorage, options)
    val module = forms.collectFirst { case Module(m) => m }.get
    val erlFile = forms.collectFirst({ case File(erlFile, _) => erlFile }).get
    val erlPath = calcSourcePath(erlFile, astStorage)

    val lines = Files.readAllLines(erlPath).asScala.toList.map(_.replace('\t', ' '))

    val invalidForms = DbApi.getInvalidForms(module).getOrElse(Nil)
    val allForms = invalidForms ++ forms

    val elpMetadata = forms.collectFirst { case md: ElpMetadata => md }
    val lineBreaks = Lines.toLineBreaks(Files.readAllBytes(erlPath))
    val (fixmedForms, redundantFixmes) = Pipeline.applyFixmes(allForms, elpMetadata)
    val statusDs = statusDiags(fixmedForms, lineBreaks)
    val errorDs = errorDiags(formsToErrors(fixmedForms) ++ redundantFixmes, lineBreaks)

    lines.zipWithIndex.map { case (text, i) =>
      val l = i + 1
      DLine(l, text, statusDs.get(l), errorDs.getOrElse(l, List.empty))
    }
  }

  private def statusDiags(forms: List[InternalForm], lineBreaks: Array[Int]): Map[Int, Status] = {
    var diags = Map.empty[InternalForm, Status]
    val invalidSpecIds = forms.collect { case InvalidFunSpec(id, _) =>
      id
    }.toSet
    for (form <- forms) form match {
      case FuncDecl(id, errors) =>
        if (invalidSpecIds(id)) ()
        else if (errors.nonEmpty) diags += form -> ErrorStatus
        else diags += form -> OkStatus
      case _: MisBehaviour   => diags += form -> ErrorStatus
      case _: InvalidForm    => diags += form -> InvalidStatus
      case _: NoSpecFuncDecl => diags += form -> NoSpecStatus
      case _                 =>
    }
    diags.map { case (form, status) => Lines.asLine(form.pos, lineBreaks) -> status }
  }

  private def errorDiags(
      errors: List[Diagnostic.Diagnostic],
      lineBreaks: Array[Int],
  ): Map[Int, List[Diagnostic.Diagnostic]] = {
    def line(te: Diagnostic.Diagnostic): Int = Lines.asLine(te.pos, lineBreaks)
    errors.groupBy(line)
  }

  private def calcSourcePath(srcFileFromForms: String, astStorage: AstStorage): Path = astStorage match {
    case DbApi.AstBeam(path) if path.isAbsolute && srcFileFromForms.endsWith(".erl") =>
      // for guessing source path when running eqWAlizer on OTP itself
      Paths.get(path.toString.replace("/ebin/", "/src/").replace(".beam", ".erl"))
    case _ =>
      Paths.get(srcFileFromForms)
  }

  private def formsToErrors(forms: List[InternalForm]) = forms.collect {
    case invalid: InvalidForm =>
      List(invalid.te)
    case MisBehaviour(te) =>
      List(te)
    case FuncDecl(_, errors) =>
      errors
  }.flatten
}
