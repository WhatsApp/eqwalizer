/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.Main

class CliSpec extends SnapshotSpec {
  describe("eqwalizer") {
    it("prints help by default") {
      checkAction(Main.main(Array()), "help.cli")
    }

    it("prints help for command that is too short") {
      checkAction(Main.main(Array("check")), "help.cli")
    }

    it("prints help for wrong command") {
      checkAction(Main.main(Array("do_it", "misc")), "help.cli")
    }

    it("type checks a single module") {
      checkAction(Main.main(Array("check", "as_pat")), "check02.cli")
    }

    it("dies hard with a missing ast file") {
      intercept[IllegalArgumentException] {
        Main.main(Array("check", "missing"))
      }
    }

    it("dies hard (for ELP) with a missing ast file") {
      intercept[IllegalArgumentException] {
        Main.main(Array("check", "--json", "missing"))
      }
    }

    it("prints ELP diagnostics") {
      checkAction(Main.main(Array("check", "refine", "--json")), "refine.elp.json")
      checkAction(Main.main(Array("check", "opaque", "--json")), "opaque.elp.json")
    }
  }
}
