/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import scala.collection.mutable

// This object proxies to existing methods inside ELP with similar signatures.
// Its purpose is to make porting to Rust more isolated and structured.
object ELPProxy {
  // the set of "loaded" modules to track dependencies
  private val modules: mutable.Set[String] = mutable.Set.empty
  // the set of used modules in the current session
  def depModules(): Set[String] = {
    val result = modules.toSet
    modules.clear()
    result
  }
  // Caches (similar to salsa caches)
  // jsoniter_scala codecs boilerplate
  // each method below has a counterpart in ELP
}
