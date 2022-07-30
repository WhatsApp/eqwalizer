/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.{Pipeline, config}
import com.whatsapp.eqwalizer.ast.{App, Forms, Id}
import com.whatsapp.eqwalizer.ast.stub.DbApi

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

object MiscInfo {
  case class ModuleInfo(name: String, exportedFuns: List[Id])
  case class AppInfo(name: String, modules: List[ModuleInfo])
  var result: ListBuffer[AppInfo] = ListBuffer.empty

  def main(args: Array[String]): Unit = {
    val fileToDump = args(0)
    val result = analyze()
    val json = toJson(result)
    val jsonStr = json.render(indent = 2)
    Files.write(Paths.get(fileToDump), jsonStr.getBytes)
  }

  private def analyze(): List[AppInfo] =
    DbApi.projectApps.values.toList.sortBy(_.name).flatMap(analyzeApp)

  private def analyzeApp(app: App): Option[AppInfo] =
    if (config.depApps(app.name))
      None
    else {
      val moduleInfos = app.modules.sorted.map(analyzeModule)
      Some(AppInfo(app.name, moduleInfos))
    }

  private def analyzeModule(module: String): ModuleInfo = {
    val listener = new MiscInfoListener
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    Pipeline.traverseForms(forms, listener)
    ModuleInfo(module, listener.getExportedFuns)
  }

  def toJson(result: List[AppInfo]): ujson.Arr =
    ujson.Arr.from(result.map(appInfoToJson))

  private def appInfoToJson(appInfo: AppInfo): ujson.Obj = {
    val name = ujson.Str(appInfo.name)
    val moduleInfos = ujson.Arr.from(appInfo.modules.map(moduleInfoToJson))
    ujson.Obj("app" -> name, "modules" -> moduleInfos)
  }

  private def moduleInfoToJson(moduleInfo: ModuleInfo): ujson.Obj = {
    val ids = moduleInfo.exportedFuns.map(id => ujson.Str(id.toString))
    val name = ujson.Str(moduleInfo.name)
    ujson.Obj("module" -> name, "exported_funs" -> ids)
  }
}
