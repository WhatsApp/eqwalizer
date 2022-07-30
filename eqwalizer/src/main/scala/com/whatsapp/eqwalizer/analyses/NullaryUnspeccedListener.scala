/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.AstListener
import com.whatsapp.eqwalizer.ast.Forms.{CompileExportAll, Export, ExternalFunSpec, FunDecl}

import java.nio.file.{Files, Paths}

class NullaryUnspeccedListener extends AstListener {
  private val generatedMark: String = "@" + "generated"
  private var module: String = null
  private var exportAll: Boolean = false
  private var generatedMod: Boolean = false
  private var funs: Set[String] =
    Set.empty
  private var specs: Set[String] =
    Set.empty
  private var exported: Set[String] =
    Set.empty

  override def enterExport(e: Export): Unit =
    exported ++= e.funs.filter(_.arity == 0).map(_.name)

  override def enterCompileExportAll(ea: CompileExportAll): Unit =
    exportAll = true

  override def enterModule(m: String, erlFile: String): Unit = {
    module = m
    val contents = new String(Files.readAllBytes(Paths.get(erlFile)))
    val preamble = contents.take(200)
    generatedMod = preamble.contains(generatedMark)
  }

  override def enterFunSpec(spec: ExternalFunSpec): Unit =
    if (spec.id.arity == 0) {
      specs += spec.id.name
    } else ()

  override def enterFunDecl(funDecl: FunDecl): Unit =
    if (funDecl.id.arity == 0) {
      funs += funDecl.id.name
    } else ()

  def getPrivateFuns: Set[String] =
    if (exportAll) Set.empty
    else (funs -- specs).filterNot(exported)

  def getExportedFuns: Set[String] = {
    if (exportAll) (funs -- specs)
    else (funs -- specs).filter(exported)
  }

  def isGenerated: Boolean =
    generatedMod
}
