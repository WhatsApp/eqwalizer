/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import java.nio.file.{Files, Paths}
import com.whatsapp.eqwalizer.ast.{ConvertAst, Lines}
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.TransitiveInvalid
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.io.{AstLoader, EData}

object Invalids {
  def main(args: Array[String]): Unit = {
    val invalids = DbApi.projectApps.values
      .flatMap(_.modules)
      .toList
      .sorted
      .flatMap { module =>
        val lineBreaks = getLinesBreaks(module)
        DbApi.getInvalidForms(module).get.map(f => (f, module, lineBreaks))
      }
    if (args.contains("-v")) {
      invalids.foreach { case (form, module, lineBreaks) =>
        val line = Lines.asLine(form.pos, lineBreaks)
        val loc = s"$module:$line".padTo(40, ' ')
        println(s"$loc ${form.te.msg}")
      }
      println(" ")
    }
    val forms = invalids.map(_._1)
    val invalidAliases = forms.collect { case form: InvalidTypeDecl => form }
    val invalidRecs = forms.collect {
      case form: InvalidRecDecl              => form
      case form: InvalidConvertTypeInRecDecl => form
    }
    val invalidSpecs = forms.collect { case form: InvalidFunSpec => form }

    def isDirect(form: InvalidForm): Boolean = form.te match {
      case TransitiveInvalid(_, _, _references) => true
      case _                                    => false
    }

    def fmt(s: String): String = s.padTo(10, ' ') + "    "
    def summarize(forms: List[InvalidForm]): String = {
      val (directs, transitives) = forms.partition(isDirect)
      s"${fmt(directs.size.toString)}${fmt(transitives.size.toString)}${fmt(forms.size.toString)}"
    }

    println(s"                              ${fmt("direct")}${fmt("transitive")}${fmt("total")}")
    println(s"invalid records             : ${summarize(invalidRecs)}")
    println(s"invalid aliases and opaques : ${summarize(invalidAliases)} ")
    println(s"invalid specs               : ${summarize(invalidSpecs)}")

  }

  private def getLinesBreaks(module: String): Array[Int] = {
    val astStorage = DbApi.getAstStorage(module).get
    val formsJ = AstLoader.loadAbstractFormsJ(astStorage)
    val fromBeam = DbApi.fromBeam(module)
    for {
      i <- 0 until formsJ.arity()
      form <- new ConvertAst(fromBeam).convertForm(EData.fromJava(formsJ.elementAt(i)))
    } form match {
      case File(erlFile, _) if erlFile.endsWith(".erl") =>
        return Lines.toLineBreaks(Files.readAllBytes(Paths.get(erlFile)))
      case _ => ()
    }
    throw new IllegalStateException("no file attribute found")
  }
}
