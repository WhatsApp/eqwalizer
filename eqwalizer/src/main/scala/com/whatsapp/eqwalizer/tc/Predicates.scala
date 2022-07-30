/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Pats.{PatAtom, PatWild}
import com.whatsapp.eqwalizer.ast.{Filters, RemoteId}

object Predicates {
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

  def isCaseIf(e: Case): Boolean = {
    val Case(sel, clauses) = e
    clauses.size == 2 && isPredicateSelector(sel) && testingBranches(clauses)
  }

  def asIf(e: Case): If = {
    val Case(sel, clauses) = e
    val List(clause1, clause2) = clauses
    val Some(test) = Filters.asTest(sel)
    clause1.pats.head match {
      case PatAtom("true") =>
        val ifClause1 = Clause(List.empty, List(Guard(List(test))), clause1.body)(clause1.pos)
        val ifClause2 = Clause(List.empty, List.empty, clause2.body)(clause2.pos)
        If(List(ifClause1, ifClause2))(e.pos)
      case _ =>
        val ifClause1 = Clause(List.empty, List(Guard(List(test))), clause2.body)(clause2.pos)
        val ifClause2 = Clause(List.empty, List.empty, clause1.body)(clause1.pos)
        If(List(ifClause1, ifClause2))(e.pos)
    }
  }

  private def isPredicateSelector(sel: Expr): Boolean =
    sel match {
      case RemoteCall(RemoteId("erlang", f, 1), Var(_) :: _) if predicates1(f) =>
        true
      case RemoteCall(RemoteId("erlang", "is_record", 2), List(Var(_), AtomLit(_))) =>
        true
      case RemoteCall(RemoteId("erlang", "is_function", 2), List(Var(_), IntLit(_))) =>
        true
      case RemoteCall(RemoteId("erlang", "is_record", 3), List(Var(_), AtomLit(_), IntLit(_))) =>
        true
      case _ =>
        false
    }

  private def testingBranches(clauses: List[Clause]): Boolean =
    clauses match {
      case List(Clause(List(pat1), List(), _), Clause(List(pat2), List(), _)) =>
        (pat1, pat2) match {
          case (PatAtom("true"), PatAtom("false") | PatWild()) => true
          case (PatAtom("false"), PatAtom("true") | PatWild()) => true
          case (_, _)                                          => false
        }
      case _ =>
        false
    }
}
