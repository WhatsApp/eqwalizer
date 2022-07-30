/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.stub.DbApi

object ModuleTypeDeps {
  val recordArity = -1
  def getTypeDeps(module: String): Map[RemoteId, List[RemoteId]] = {
    val forms = DbApi.loadStubForms(module).get
    val listener = new TypeDepsListener
    Pipeline.traverseForms(forms, listener)
    listener.getDeps
  }
}
