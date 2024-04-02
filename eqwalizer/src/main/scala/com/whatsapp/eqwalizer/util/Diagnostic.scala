/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Pos

object Diagnostic {
  trait Diagnostic {
    val pos: Pos
    val msg: String
    def explanation: Option[String] = None
    def errorName: String // stable identifier for the class of error, to be used in metrics
    def docURL: String = s"https://fb.me/eqwalizer_errors#$errorName"
    def erroneousExpr: Option[Expr]
  }
}
