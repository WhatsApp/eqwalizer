/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Pats.{PatAtom, PatWild}
import com.whatsapp.eqwalizer.ast.Filters

object Predicates {
  def isCaseIf(e: Case): Boolean = {
    val Case(sel, clauses) = e
    clauses.size == 2 && booleanClauses(clauses) && Filters.asTest(sel).isDefined
  }

  def asIf(e: Case): If = {
    val Case(sel, clauses) = e
    val List(clause1, clause2) = clauses
    val Some(test) = Filters.asTest(sel): @unchecked
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

  def booleanClauses(clauses: List[Clause]): Boolean =
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

  def posNegClauses(clauses: List[Clause]): (Clause, Clause) = {
    val List(clause1 @ Clause(List(pat1), List(), _), clause2 @ Clause(List(pat2), List(), _)) = clauses
    (pat1, pat2) match {
      case (PatAtom("true"), PatAtom("false") | PatWild()) =>
        (clause1, clause2)
      case (_, _) =>
        (clause2, clause1)
    }
  }

  def booleanReturnClauses(clauses: List[Clause]): Boolean =
    clauses match {
      case List(Clause(_, _, Body(List(expr1))), Clause(_, _, Body(List(expr2)))) =>
        (expr1, expr2) match {
          case (AtomLit("true"), AtomLit("false")) => true
          case (AtomLit("false"), AtomLit("true")) => true
          case (_, _)                              => false
        }
      case _ =>
        false
    }

  def getTrueFalseReturnClauses(clauses: List[Clause]): (Clause, Clause) = {
    val List(clause1 @ Clause(_, _, Body(List(expr1))), clause2 @ Clause(_, _, Body(List(expr2)))) = clauses
    (expr1, expr2) match {
      case (AtomLit("true"), AtomLit("false")) => (clause1, clause2)
      case (_, _)                              => (clause2, clause1)
    }
  }
}
