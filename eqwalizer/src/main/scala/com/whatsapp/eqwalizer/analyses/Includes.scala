/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, AstListener, Forms}
import com.whatsapp.eqwalizer.io.Project

object Includes {
  type Line = Int
  case class IncludesInfo(file: String, includes: List[(Line, String)])

  def main(args: Array[String]): Unit = {
    val output = args.head
    val includes = analyze()
    val json = ujson.Arr.from(includes.map(includesInfoToJson))
    val jsonText = json.render(indent = 2)
    Files.writeString(Paths.get(output), jsonText)
  }

  private def analyze(): List[IncludesInfo] =
    DbApi.projectApps.values.flatMap(analyzeApp).toList.sortBy(_.file)

  private def analyzeApp(app: App): List[IncludesInfo] =
    app.modules.map(analyzeModule).collect {
      case Some(info) if info.includes.nonEmpty => info
    }

  private def analyzeModule(module: String): Option[IncludesInfo] = {
    DbApi.getBeamAstStorage(module) match {
      case Some(astStorage) =>
        val forms = Forms.load(astStorage)
        val listener = new FileListener()
        Pipeline.traverseForms(forms, listener)
        Some(listener.getIncludesInfo)
      case None => None
    }
  }

  private def includesInfoToJson(includesInfo: IncludesInfo): ujson.Obj = {
    val includes = ujson.Arr.from(includesInfo.includes.map({ case (line, include) =>
      ujson.Obj("line" -> ujson.Num(line), "target_path" -> ujson.Str(include))
    }))
    ujson.Obj("file" -> ujson.Str(includesInfo.file), "includes" -> includes)
  }

  class FileListener extends AstListener {
    var currentFile: String = _
    var prevFile: Forms.File = _
    val mapping: ListBuffer[(Line, String)] = ListBuffer.empty

    override def enterFile(file: Forms.File): Unit = {
      if (currentFile == null)
        currentFile = file.file
      else if (currentFile == file.file)
        mapping.addOne((file.start - 1, Project.relativePath(prevFile.file)))
      else
        prevFile = file
    }

    def getIncludesInfo: IncludesInfo =
      IncludesInfo(Project.relativePath(currentFile), mapping.toList)
  }
}
