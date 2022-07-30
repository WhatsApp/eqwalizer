/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.ExternalTypes.ExtType
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Guards.{Guard, Test}
import com.whatsapp.eqwalizer.ast.Pats.Pat

trait AstListener {
  // $COVERAGE-OFF$
  def enterClause(clause: Clause): Unit = ()
  def exitClause(clause: Clause): Unit = ()

  def enterExpr(e: Expr): Unit = ()
  def exitExpr(e: Expr): Unit = ()

  def enterFunDecl(funDecl: FunDecl): Unit = ()
  def exitFunDecl(funDecl: FunDecl): Unit = ()

  def enterFunSpec(spec: ExternalFunSpec): Unit = ()
  def exitFunSpec(spec: ExternalFunSpec): Unit = ()

  def enterGuard(guard: Guard): Unit = ()
  def exitGuard(guard: Guard): Unit = ()

  def enterModule(m: String, erlFile: String): Unit = ()
  def exitModule(m: String): Unit = ()

  def enterPat(pat: Pat): Unit = ()
  def exitPat(pat: Pat): Unit = ()

  def enterTest(test: Test): Unit = ()
  def exitTest(test: Test): Unit = ()

  def enterTypeDecl(tp: ExternalTypeDecl): Unit = ()
  def exitTypeDecl(tp: ExternalTypeDecl): Unit = ()

  def enterOpaqueDecl(tp: ExternalOpaqueDecl): Unit = ()
  def exitOpaqueDecl(tp: ExternalOpaqueDecl): Unit = ()

  def enterType(tp: ExtType): Unit = ()
  def exitType(tp: ExtType): Unit = ()

  def enterRecDecl(recDecl: ExternalRecDecl): Unit = ()
  def exitRecDecl(recDecl: ExternalRecDecl): Unit = ()

  def enterCompileExportAll(exportAll: CompileExportAll): Unit = ()
  def exitCompileExportAll(exportAll: CompileExportAll): Unit = ()

  def enterExport(e: Export): Unit = ()
  def exitExport(e: Export): Unit = ()

  def enterFile(file: File): Unit = ()
  def exitFile(file: File): Unit = ()

  def enterTypingAttribute(typing: TypingAttribute): Unit = ()
  def exitTypingAttribute(typing: TypingAttribute): Unit = ()

  def traverseHeaders(): Boolean = false
}
