/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.config

import java.nio.file.Paths

class DbSpec extends org.scalatest.funspec.AnyFunSpec {
  describe("module DB") {
    it("should locate ast files from test projects") {
      val expPath =
        if (config.useElp) "test_projects/.ast/misc.etf"
        else "test_projects/_build/default/lib/check/ebin/misc.beam"
      val expAbsPath = Paths.get(expPath).toAbsolutePath.toString
      val actualAbsPath = DbApi.getAstStorage("misc").map {
        case DbApi.AstBeam(path)    => path.toAbsolutePath.toString
        case DbApi.AstEtfFile(path) => path.toAbsolutePath.toString
        case DbApi.AstEtfIpc(_)     => throw new IllegalStateException("ipc mode is not expected in tests")
        case DbApi.AstJsonIpc(_)    => throw new IllegalStateException("ipc mode is not expected in tests")
      }
      assert(actualAbsPath === Some(expAbsPath))
    }

    it("should not locate unknown modules") {
      assert(DbApi.getAstStorage("unknown_module") === None)
    }
  }
}
