/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.Forms.ExternalFunSpec
import com.whatsapp.eqwalizer.ast.{ExtModuleStub, Id}
import com.whatsapp.eqwalizer.ast.stub.DbApi

class CustomSpecsSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("custom specs") {
    val stub = DbApi.getExtModuleStub("eqwalizer_specs").get
    val funIds = getFunIds(stub)
    funIds.foreach(testSpec)
    val overloadedFunIds = getOverloadedFunIds(stub)
    overloadedFunIds.foreach(testOverloadedSpec)
  }

  private def testSpec(stubId: Id): Unit = {
    it(stubId.toString) {
      val Array(module, funName) = stubId.name.split(":")
      val id = Id(funName, stubId.arity)
      val spec1 = DbApi.getSpec(module, id).get
      val spec2 = DbApi.getSpec("eqwalizer_specs", stubId).get
      assert(spec1.ty === spec2.ty)
    }
  }

  private def testOverloadedSpec(stubId: Id): Unit = {
    it(stubId.toString) {
      val Array(module, funName) = stubId.name.split(":")
      val id = Id(funName, stubId.arity)
      val spec1 = DbApi.getOverloadedSpec(module, id).get
      val spec2 = DbApi.getOverloadedSpec("eqwalizer_specs", stubId).get
      assert(spec1.tys === spec2.tys)
    }
  }

  private def getFunIds(stub: ExtModuleStub): List[Id] =
    stub.forms.collect { case ExternalFunSpec(id, tys) if tys.size == 1 => id }

  private def getOverloadedFunIds(stub: ExtModuleStub): List[Id] =
    stub.forms.collect { case ExternalFunSpec(id, tys) if tys.size != 1 => id }
}
