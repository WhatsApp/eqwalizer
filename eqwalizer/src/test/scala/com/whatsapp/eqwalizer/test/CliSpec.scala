/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.Main
import com.whatsapp.eqwalizer.analyses._

import java.nio.file.Files

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

    it("runs smoke checking") {
      checkAction(Main.main(Array("smoke", "--no-progress")), "smoke.cli")
    }

    it("reports checkable funs summary") {
      checkAction(CheckableFuns.main(Array()), "checkable_funs.cli")
    }

    it("reports pinned vars") {
      checkAction(PinnedVars.main(Array()), "pinned.cli")
    }

    it("reports refined record types") {
      checkAction(RefinedRecordTypes.main(Array()), "refined_record_types.cli")
    }

    it("reports op types") {
      checkAction(OpTypes.main(Array()), "op_types.cli")
    }

    it("reports any arity fun types") {
      checkAction(AnyArityFunTypes.main(Array()), "any_arity_fun_types.cli")
    }

    it("report bad prop types") {
      checkAction(BadPropTypes.main(Array()), "bad_prop_types.cli")
    }

    it("reports approximated maps") {
      checkAction(ApproximatedMaps.main(Array()), "approximated_maps.cli")
    }

    it("reports overloaded fun specs") {
      checkAction(OverloadedFunSpecs.main(Array()), "overloaded_fun_specs.cli")
    }

    it("reports unions with type variables") {
      checkAction(UnionsWithTypeVars.main(Array()), "unions_with_type_vars.cli")
    }

    it("reports generic overloaded specs") {
      checkAction(GenericOverloadedFunSpecs.main(Array()), "generic_overloaded_fun_specs.cli")
    }

    it("reports fun overloaded specs") {
      checkAction(FunOverloadedFunSpecs.main(Array()), "fun_overloaded_fun_specs.cli")
    }

    it("reports FIXME specs") {
      checkAction(DiscardedSpecs.main(Array()), "discarded_specs.cli")
    }

    it("reports tryOf expressions") {
      checkAction(TryOf.main(Array()), "try_of.cli")
    }

    it("reports clauses with repeated variable names") {
      checkAction(RepeatedVars.main(Array()), "repeated_vars.cli")
    }

    it("reports unspecced callback implementations") {
      checkAction(UnspeccedCallbackImpls.main(Array()), "unspecced_callback_impls.cli")
    }

    it("reports OTP function calls") {
      checkAction(OTPFuns.main(Array()), "otp_funs.cli")
    }

    it("runs MiscInfo utility") {
      val tmpdir = Files.createTempDirectory("tmpDirPrefix").toFile.getAbsolutePath
      val tmpFile = s"$tmpdir/misc.json"
      checkActionFile(MiscInfo.main(Array(tmpFile)), actualFile = tmpFile, expFile = "misc.json")
    }

    it("dumps resolved includes") {
      val tmpdir = Files.createTempDirectory("tmpDirPrefix").toFile.getAbsolutePath
      val tmpFile = s"$tmpdir/includes.json"
      checkActionFile(Includes.main(Array(tmpFile)), actualFile = tmpFile, expFile = "includes.json")
    }

    it("reports lambdas summary") {
      checkAction(Lambdas.main(Array()), "lambdas.cli")
    }
    it("reports long error messages") {
      checkAction(LongErrors.main(Array()), "long_errors.cli")
    }
    it("reports forms") {
      checkAction(Invalids.main(Array()), "invalids.cli")
    }

    it("prints ELP diagnostics") {
      checkAction(Main.main(Array("check", "refine", "--json")), "refine.elp.json")
      checkAction(Main.main(Array("check", "opaque", "--json")), "opaque.elp.json")
    }

    it("prints stats") {
      val modules = Set("fun_stats", "fun_stats2", "check_SUITE", "fault_tolerance")

      def transform(value: ujson.Value): ujson.Value = {
        val map = value.asInstanceOf[ujson.Obj].value.toMap
        val transformedMap = map.view.filterKeys(modules)
        ujson.Obj.from(transformedMap)
      }

      checkJsonAction(Main.main(Array("stats")), "stats.json", transform)
    }
  }
}
