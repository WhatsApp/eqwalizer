/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Exprs.{LocalCall, LocalFun}
import com.whatsapp.eqwalizer.ast.Forms.FunDecl
import com.whatsapp.eqwalizer.ast.{AstListener, Exprs, RemoteId}

import scala.collection.mutable.ListBuffer

class FunDepsListener extends AstListener {
  def getDeps: Map[RemoteId, List[RemoteId]] = deps
  private var currentFun: Option[RemoteId] = None
  private var module: Option[String] = None
  private val currentDeps: ListBuffer[RemoteId] = ListBuffer.empty
  private var deps: Map[RemoteId, List[RemoteId]] = Map.empty

  override def enterModule(m: String, erlFile: String): Unit =
    module = Some(m)

  override def enterFunDecl(funDecl: FunDecl): Unit =
    currentFun = Some(RemoteId(module.get, funDecl.id.name, funDecl.id.arity))

  override def exitFunDecl(funDecl: FunDecl): Unit = {
    deps += currentFun.get -> currentDeps.toList
    currentFun = None
    currentDeps.clear()
  }

  override def enterExpr(e: Exprs.Expr): Unit =
    e match {
      case LocalCall(lid, _) =>
        currentDeps += RemoteId(module.get, lid.name, lid.arity)
      case LocalFun(lid) =>
        currentDeps += RemoteId(module.get, lid.name, lid.arity)
      case Exprs.RemoteCall(rid, _) =>
        currentDeps += rid
      case Exprs.RemoteFun(rid) =>
        currentDeps += rid
      case _ =>
    }
}
