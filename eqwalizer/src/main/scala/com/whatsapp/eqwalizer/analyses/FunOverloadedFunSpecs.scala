/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.{App, RemoteId, Types}
import com.whatsapp.eqwalizer.ast.Forms.OverloadedFunSpec
import com.whatsapp.eqwalizer.ast.Types.{AnyArityFunType, AnyFunType, FunType}
import com.whatsapp.eqwalizer.ast.stub.DbApi

object FunOverloadedFunSpecs {
  def main(args: Array[String]): Unit =
    analyze()

  private def analyze(): Unit = {
    val apps = DbApi.projectApps.values.toList.sortBy(_.name)
    val funIds = apps.flatMap(analyzeApp).map(_.toString).sorted
    funIds.foreach(Console.println)
  }

  def analyzeApp(app: App): Iterable[RemoteId] =
    app.modules.sorted.flatMap(analyzeModule)

  private def analyzeModule(module: String): Iterable[RemoteId] =
    DbUtil.getOverloadedSpecs(module).get.flatMap { case (id, spec) =>
      if (hasFun(spec)) Some(RemoteId(module, id.name, id.arity)) else None
    }

  private def hasFun(spec: OverloadedFunSpec): Boolean = {
    val funListener = new FunTypeListener()
    val traverse = new TypeTraverse(funListener)
    spec.tys.foreach { ft =>
      ft.argTys.foreach(traverse.traverse)
      traverse.traverse(ft.resTy)
    }
    funListener.hasFun
  }

  private class FunTypeListener() extends TypeListener {
    var hasFun: Boolean = false
    override def enterType(tp: Types.Type): Unit = tp match {
      case FunType(_, _, _) | AnyFunType | AnyArityFunType(_) =>
        hasFun = true
      case _ =>
    }
  }
}
