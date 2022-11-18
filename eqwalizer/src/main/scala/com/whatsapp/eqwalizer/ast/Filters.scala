/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Guards._

object Filters {
  val predicates1: Set[String] =
    Set(
      "is_atom",
      "is_binary",
      "is_bitstring",
      "is_boolean",
      "is_float",
      "is_function",
      "is_integer",
      "is_list",
      "is_number",
      "is_pid",
      "is_port",
      "is_reference",
      "is_map",
      "is_tuple",
    )

  val guards_binop: Set[String] =
    Set(
      "/",
      "*",
      "-",
      "+",
      "div",
      "rem",
      "band",
      "bor",
      "bxor",
      "bsl",
      "bsr",
      "or",
      "xor",
      "and",
      ">=",
      ">",
      "=<",
      "<",
      "/=",
      "=/=",
      "==",
      "=:=",
      "andalso",
      "orelse",
    )

  val guards_unop: Set[String] =
    Set("bnot", "+", "-", "not")

  private def isPredicateFun(name: String, arity: Int): Boolean =
    (name, arity) match {
      case (_, 1) =>
        predicates1(name)
      case ("is_record", 2) =>
        true
      case ("is_function", 2) =>
        true
      case ("is_record", 3) =>
        true
      case _ =>
        false
    }

  def asTest(expr: Expr): Option[Test] =
    expr match {
      case Var(n) =>
        Some(TestVar(n)(expr.pos))
      case AtomLit(s) =>
        Some(TestAtom(s)(expr.pos))
      case IntLit(i) =>
        Some(TestNumber(Some(i))(expr.pos))
      case RemoteCall(RemoteId("erlang", f, arity), args) if isPredicateFun(f, arity) =>
        for {
          argsT <- asTests(args)
        } yield TestCall(Id(f, arity), argsT)(expr.pos)
      case UnOp(op, arg) if guards_unop(op) =>
        for {
          argT <- asTest(arg)
        } yield TestUnOp(op, argT)(expr.pos)
      case BinOp(op, arg1, arg2) if guards_binop(op) =>
        for {
          arg1T <- asTest(arg1)
          arg2T <- asTest(arg2)
        } yield TestBinOp(op, arg1T, arg2T)(expr.pos)
      case _ =>
        None
    }

  def asTests(exprs: List[Expr]): Option[List[Test]] = {
    val maybeRes = exprs.flatMap(asTest)
    if (maybeRes.length == exprs.size)
      Some(maybeRes)
    else
      None
  }
}
