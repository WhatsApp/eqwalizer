package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.analyses.DbgTraceCheck
import com.whatsapp.eqwalizer.analyses.DbgTraceCheck._
import com.whatsapp.eqwalizer.ast.Id
import org.scalatest.funspec.AnyFunSpec

object DbgTraceCheckSpec {
  sealed trait ShortError
  case class Res(module: String, id: Id) extends ShortError
  case class Arg(module: String, id: Id, index: Int) extends ShortError

  def check(traceFile: String): List[ShortError] =
    DbgTraceCheck.check(traceFile) map {
      case ResultError(module, id, _value) =>
        Res(module, id)
      case ArgumentError(module, id, index, _value) =>
        // human-readable indices for making tests simpler
        Arg(module, id, index + 1)
    }
}

class DbgTraceCheckSpec extends AnyFunSpec {
  import DbgTraceCheckSpec._
  describe("DbgTraceCheck") {
    it("reports basic spec violations found in dbg trace file") {
      val actualErrors = check("test_projects/traces/base.dbg_trace")
      val module = "base_trace_tests"
      val expectedErrors =
        List(
          Arg(module, Id("case01_bad_arg1", 1), 1),
          Arg(module, Id("case02_bad_arg2", 2), 2),
          Arg(module, Id("case03_bad_arg23", 3), 2),
          Arg(module, Id("case03_bad_arg23", 3), 3),
          Arg(module, Id("case07_bad_arg123", 3), 1),
          Arg(module, Id("case07_bad_arg123", 3), 2),
          Arg(module, Id("case07_bad_arg123", 3), 3),
          Res(module, Id("case08_bad_res", 0)),
          Res(module, Id("case10_overload_bad_res", 1)),
        )
      assert(actualErrors == expectedErrors)
    }

    it("type checks values") {
      val actualErrors = check("test_projects/traces/type.dbg_trace")
      val module = "type_trace_tests"
      val expectedErrors =
        List(
          Res(module, Id("none_err", 0)),
          Res(module, Id("atom_lit_err1", 0)),
          Res(module, Id("atom_lit_err2", 0)),
          Res(module, Id("any_fun_err", 0)),
          Res(module, Id("fun_err", 0)),
          Res(module, Id("any_tuple_err", 0)),
          Res(module, Id("tuple_err1", 0)),
          Res(module, Id("tuple_err2", 0)),
          Res(module, Id("nil_err", 0)),
          Res(module, Id("list_err1", 0)),
          Res(module, Id("list_err2", 0)),
          Res(module, Id("binary_err", 0)),
          Res(module, Id("pid_err", 0)),
          Res(module, Id("number_err", 0)),
          Res(module, Id("union_err", 0)),
          Res(module, Id("reference_err", 0)),
          Res(module, Id("record_err1", 0)),
          Res(module, Id("record_err2", 0)),
          Res(module, Id("record_err3", 0)),
          Res(module, Id("dictionary_err1", 0)),
          Res(module, Id("dictionary_err2", 0)),
          Res(module, Id("dictionary_err3", 0)),
          Res(module, Id("shape_err1", 0)),
          Res(module, Id("shape_err2", 0)),
          Res(module, Id("shape_err3", 0)),
          Res(module, Id("shape_err4", 0)),
          Res(module, Id("shape_err5", 0)),
          Res(module, Id("opaque_err1", 0)),
          Res(module, Id("opaque_err2", 0)),
        )
      assert(actualErrors == expectedErrors)
    }
  }
}
