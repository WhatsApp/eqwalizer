/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{AstListener, Id}

import scala.collection.mutable.ListBuffer

// A very specialised analysis - to dump
// exported funs for a current module.
class MiscInfoListener extends AstListener {
  private var exportAll: Boolean =
    false
  private val explicitlyExportedFuns: ListBuffer[Id] =
    ListBuffer.empty
  private val allFuns: ListBuffer[Id] =
    ListBuffer.empty

  def getExportedFuns: List[Id] =
    if (exportAll)
      allFuns.toList
    else
      explicitlyExportedFuns.toList

  override def enterExport(e: Export): Unit =
    ()
  override def exitExport(e: Export): Unit =
    explicitlyExportedFuns.addAll(e.funs)

  override def enterCompileExportAll(exportAll: CompileExportAll): Unit =
    ()
  override def exitCompileExportAll(exportAll: CompileExportAll): Unit =
    this.exportAll = true

  override def enterFunDecl(funDecl: FunDecl): Unit = ()
  override def exitFunDecl(funDecl: FunDecl): Unit =
    allFuns.addOne(funDecl.id)
}
