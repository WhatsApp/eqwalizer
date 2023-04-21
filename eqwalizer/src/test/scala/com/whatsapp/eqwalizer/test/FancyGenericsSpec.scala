/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.test

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Pats.PatVar
import com.whatsapp.eqwalizer.ast.{TextRange, RemoteId, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.{Env, PipelineContext}
import org.scalatest.funspec.AnyFunSpec

/** Test that we handle nested foralls appropriately.
  * I couldn't figure out how to generate functions with nested foralls in snapshot tests,
  * since we treat all specs as in prenex form. It may be possible to generate them
  * internally when handling higher-order functions with type paramters.
  */
class FancyGenericsSpec extends AnyFunSpec {
  private val ctx = PipelineContext("example")
  import ctx._
  def varN(v: Int): VarType = VarType(v)(name = "X")
  def idFun(v: Int): FunType =
    FunType(List(v), List(varN(v)), varN(v))

  describe("Generic features hard to cover with snapshot tests: nested foralls") {
    it("Functions identical up to renaming are Subtype.eqv") {
      val ft1 = FunType(List(1), List(idFun(0), varN(1)), varN(1))
      val ft2 = FunType(List(20), List(idFun(19), varN(20)), varN(20))
      assert(subtype.eqv(ft1, ft2))
    }
    it("Subtype.subtype returns false for functions with a different number of foralls") {
      val withForall = FunType(List(1), List(AnyType), AnyType)
      val withoutForall = FunType(Nil, List(AnyType), AnyType)
      assert(!subtype.subType(withForall, withoutForall))
      assert(!subtype.subType(withoutForall, withForall))
    }
    it("Handles vars from nested foralls gracefully (promotion)") {
      // fun<T, U>(T, U) -> T
      val fst = FunType(List(0, 1), List(varN(0), varN(1)), varN(0))
      // fun<T, U>(T, U) -> U
      val snd = FunType(List(0, 1), List(varN(0), varN(1)), varN(1))

      val f1 = FunType(List(4), List(FunType(List(0, 1), List(varN(4), varN(0)), varN(4)), varN(4)), varN(4))

      val pos = TextRange.fake
      val fId = RemoteId("fancy_generics", "f", 2)
      val funExpr = RemoteFun(fId)(pos)
      val lambda = {
        val patVars = List(PatVar("T")(pos), PatVar("U")(pos))
        val vars = List(Var("T")(pos), Var("U")(pos))
        val app = RemoteCall(fId, vars)(pos)
        val clause = Clause(patVars, Nil, Body(List(app)))(pos)
        Lambda(List(clause))(pos, name = None)
      }
      val numExpr = IntLit(Some(2))(TextRange.fake)

      val elabApply = ctx.elabApply

      elabApply.elabApply(f1, List(funExpr, numExpr), List(check.freshen(snd), NumberType), Env.empty)

      elabApply.elabApply(f1, List(lambda, numExpr), List(check.freshen(snd), NumberType), Env.empty)

      val f2 = FunType(List(4), List(fst, varN(4)), varN(4))

      val fResTy =
        elabApply.elabApply(f2, List(funExpr, numExpr), List(check.freshen(fst), NumberType), Env.empty)
      assert(
        subtype.eqv(
          fResTy,
          NumberType,
        )
      )
    }
    it("TypeVars.conformForalls does not change variable binding structure") {
      def bindingDepthsRev(ty: Type, paramToDepth: Map[Int, Int]): List[Int] = ty match {
        case FunType(foralls, argTys, resTy) =>
          val paramToDepth1 = paramToDepth.map { case (k, v) => (k, v + 1) } ++ foralls.map(_ -> 0)
          argTys.flatMap(bindingDepthsRev(_, paramToDepth1)) ++ bindingDepthsRev(resTy, paramToDepth1)
        case tv: VarType =>
          paramToDepth.get(tv.n) match {
            case Some(depth) => depth :: Nil
            case None        => Nil
          }
        case _ => TypeVars.children(ty).flatMap(bindingDepthsRev(_, paramToDepth))
      }

      def bindingDepths(ft: FunType) = bindingDepthsRev(ft, Map.empty).reverse

      val f1 = FunType(
        List(0, 1),
        List(
          // varN(0) refers to forall in current function, varN(1) refers to 1 in outer binding
          FunType(List(0), List(varN(0), varN(1)), varN(0)),
          varN(1),
        ),
        varN(0),
      )

      // no shadowing - everything should be at depth 0
      val f2 = FunType(
        List(2, 2),
        List(
          FunType(List(0), List(varN(0), varN(0)), varN(0)),
          varN(2),
        ),
        varN(2),
      )

      val Some((newF1, newF2)) = TypeVars.conformForalls(f1, f2)
      assert(bindingDepths(f1) == bindingDepths(newF1))
      assert(bindingDepths(f2) == bindingDepths(newF2))
    }

    it("TypeVars.conformForalls handles unused type vars (impossible currently) gracefully") {
      val ft1 = FunType(List(0, 1), List(AtomType), VarType(0)("x"))
      val ft2 = FunType(List(5, 6), List(AtomType), AtomType)
      TypeVars.conformForalls(ft1, ft2)
    }

  }
}
