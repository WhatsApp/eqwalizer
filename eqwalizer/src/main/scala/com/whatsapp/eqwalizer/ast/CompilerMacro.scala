/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

object CompilerMacro {
  val fake_module = "$compiler_macro"

  val funs: Set[Id] = Set(
    Id("record_info", 2)
  )
}
