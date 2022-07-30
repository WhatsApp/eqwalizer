/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.{App, Forms, Id}
import com.whatsapp.eqwalizer.ast.stub.DbApi

object UnspeccedCallbackImpls {
  case class ModuleInfo(module: String, generated: Boolean, unspeccedCallbackImpls: Set[Id])
  def main(args: Array[String]): Unit =
    analyze()

  private def analyze(): Unit =
    analyzeApps(DbApi.projectApps.values.toList)

  private def analyzeApps(apps: List[App]): Unit = {
    val infos: List[ModuleInfo] = apps.flatMap(analyzeApp).sortBy(_.module)
    val (genInfos, manInfos) = infos.partition(_.generated)

    for { i <- genInfos } if (i.unspeccedCallbackImpls.nonEmpty) {
      Console.println(i.module)
      val ids = i.unspeccedCallbackImpls.toList.map(_.toString).sorted
      val idsString = ids.mkString(", ")
      Console.println(s"  $idsString")
    }
    for { i <- manInfos } if (i.unspeccedCallbackImpls.nonEmpty) {
      Console.println(i.module)
      val ids = i.unspeccedCallbackImpls.toList.map(_.toString).sorted
      val idsString = ids.mkString(", ")
      Console.println(s"  $idsString")
    }

    val genCount = genInfos.map(_.unspeccedCallbackImpls.size).sum
    val manCount = manInfos.map(_.unspeccedCallbackImpls.size).sum
    Console.println(s"Gen implementations: $genCount")
    Console.println(s"Man implementations: $manCount")
  }

  private def analyzeApp(app: App): Iterable[ModuleInfo] =
    app.modules.sorted.map(analyzeModule)

  private def analyzeModule(module: String): ModuleInfo = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    val listener = new UnspeccedCallbackImplsListener(module)
    Pipeline.traverseForms(forms, listener)
    val generated = listener.isGenerated
    val ids = listener.getUnspeccedCallbackImpls
    ModuleInfo(module, generated, ids)
  }
}
