/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.OverloadedFunSpec
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{App, RemoteId}

object GenericOverloadedFunSpecs {
  def main(args: Array[String]): Unit =
    analyze()

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    val genericIds = apps.flatMap(analyzeApp).map(_.toString).sorted
    genericIds.foreach(Console.println)
  }

  def analyzeApp(app: App): Iterable[RemoteId] =
    app.modules.sorted.flatMap(analyzeModule)

  private def analyzeModule(module: String): Iterable[RemoteId] =
    DbUtil.getOverloadedSpecs(module).get.flatMap { case (id, spec) =>
      if (isGeneric(spec)) Some(RemoteId(module, id.name, id.arity)) else None
    }

  private def isGeneric(spec: OverloadedFunSpec): Boolean =
    spec.tys.exists(_.forall.nonEmpty)
}
