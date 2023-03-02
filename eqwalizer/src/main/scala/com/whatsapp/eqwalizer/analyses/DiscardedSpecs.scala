/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.ExternalFunSpec
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, RemoteId}

object DiscardedSpecs {
  def main(args: Array[String]): Unit = {
    val includeOtp = args.contains("-otp")
    analyze(includeOtp)
  }

  private def analyze(includeOtp: Boolean): Unit = {
    if (includeOtp) analyzeApps(DbApi.otpApps.values.toList, "OTP")
    analyzeApps(DbApi.projectApps.values.toList, "Project")
  }

  private def analyzeApps(apps: List[App], title: String): Unit = {
    val count = apps.map(countApp).sum
    val fixMeIds = apps.flatMap(analyzeApp).map(_.toString).sorted
    val fixMeRatio: Double =
      if (fixMeIds.isEmpty) 0 else fixMeIds.size * 100.0 / count
    Console.println(s"=== $title ===")
    fixMeIds.foreach(Console.println)
    Console.println(s"Discarded specs: ${fixMeIds.size}")
    Console.println(s"Total specs: $count")
    Console.println(f"Discarded ratio: $fixMeRatio%2.4f %%")
  }

  private def analyzeApp(app: App): Iterable[RemoteId] =
    app.modules.sorted.flatMap(analyzeModule)

  private def analyzeModule(module: String): Set[RemoteId] = {
    val inputStub = DbApi.getExtModuleStub(module).get

    val inputIds = inputStub.forms.collect { case ExternalFunSpec(id, _) => id }.toSet
    val validIds = DbUtil.getSpecs(module).get.keySet ++ DbUtil.getOverloadedSpecs(module).get.keySet

    val diff = inputIds -- validIds
    diff.map(id => RemoteId(module, id.name, id.arity))
  }

  private def countApp(app: App): Int =
    app.modules.map(countModule).sum

  private def countModule(module: String): Int =
    DbApi.getExtModuleStub(module).get.forms.count(_.isInstanceOf[ExternalFunSpec])
}
