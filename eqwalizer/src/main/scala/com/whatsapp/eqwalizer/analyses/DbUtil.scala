/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.{ExternalFunSpec, FunSpec, OverloadedFunSpec}
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.stub.DbApi

object DbUtil {
  def getSpecs(module: String): Option[Map[Id, FunSpec]] = {
    for {
      eStub <- DbApi.getExtModuleStub(module)
      ids = eStub.forms.collect { case ExternalFunSpec(id, _) => id }
    } yield ids.flatMap(id => DbApi.getSpec(module, id).map(id -> _)).toMap
  }

  def getOverloadedSpecs(module: String): Option[Map[Id, OverloadedFunSpec]] = {
    for {
      eStub <- DbApi.getExtModuleStub(module)
      ids = eStub.forms.collect { case ExternalFunSpec(id, _) => id }
    } yield ids.flatMap(id => DbApi.getOverloadedSpec(module, id).map(id -> _)).toMap
  }
}
