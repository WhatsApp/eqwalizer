/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.{Forms, RemoteId}

object ModuleFunDeps {
  def getFunDeps(module: String): Map[RemoteId, List[RemoteId]] = {
    val path = DbApi.getAstStorage(module).get
    val forms = Forms.load(path)
    val listener = new FunDepsListener
    Pipeline.traverseForms(forms, listener)
    listener.getDeps
  }
}
