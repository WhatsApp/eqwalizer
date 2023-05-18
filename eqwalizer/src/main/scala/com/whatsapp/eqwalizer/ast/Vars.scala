/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.tc.PipelineContext

object Vars {

  def patVars(pat: Pat): Set[String] =
    pat match {
      case PatWild() =>
        Set.empty
      case PatMatch(pat, arg) =>
        patVars(pat) ++ patVars(arg)
      case PatTuple(elems) =>
        elems.flatMap(patVars).toSet
      case PatString() =>
        Set.empty
      case PatNil() =>
        Set.empty
      case PatCons(h, t) =>
        patVars(h) ++ patVars(t)
      case PatNumber() | PatInt() =>
        Set.empty
      case PatAtom(_) =>
        Set.empty
      case PatVar(n) =>
        Set(n)
      case PatUnOp(_, arg) =>
        patVars(arg)
      case PatBinOp(_, arg1, arg2) =>
        patVars(arg1) ++ patVars(arg2)
      case PatBinary(elems) =>
        elems.flatMap(binaryElemVars).toSet
      case PatRecord(_, fields, gen) =>
        gen.toList.flatMap(patVars).toSet ++ fields.flatMap(f => patVars(f.pat)).toSet
      case PatRecordIndex(_, _) =>
        Set.empty
      case PatMap(kvs) =>
        kvs.flatMap(kv => patVars(kv._2)).toSet
    }

  private def binaryElemVars(elem: PatBinaryElem): Set[String] =
    patVars(elem.pat)

  private def binaryElemVars(elem: BinaryElem): Set[String] = {
    val sizeVars: Set[String] = elem.size.map(exprVars).getOrElse(Set.empty)
    sizeVars ++ exprVars(elem.expr)
  }

  private def bodyVars(body: Body): Set[String] =
    body.exprs.map(exprVars).reduce(_ ++ _)

  private def exprVars(expr: Expr): Set[String] = expr match {
    case Var(_) =>
      Set.empty
    case AtomLit(_) =>
      Set.empty
    case IntLit(_) | FloatLit() =>
      Set.empty
    case Block(body) =>
      bodyVars(body)
    case Match(pat, expr) =>
      patVars(pat) ++ exprVars(expr)
    case Tuple(elems) =>
      elems.flatMap(exprVars).toSet
    case StringLit(_) =>
      Set.empty
    case NilLit() =>
      Set.empty
    case Cons(h, t) =>
      exprVars(h) ++ exprVars(t)
    case Case(expr, clauses) =>
      exprVars(expr) ++ clausesVars(clauses)
    case If(clauses) =>
      clausesVars(clauses)
    case LocalCall(id, args) =>
      args.flatMap(exprVars).toSet
    case RemoteCall(id, args) =>
      args.flatMap(exprVars).toSet
    case DynCall(f, args) =>
      (f :: args).flatMap(exprVars).toSet
    case LocalFun(id) =>
      Set.empty
    case RemoteFun(id) =>
      Set.empty
    case DynRemoteFun(mod, name) =>
      exprVars(mod) ++ exprVars(name)
    case DynRemoteFunArity(mod, name, num) =>
      exprVars(mod) ++ exprVars(name) ++ exprVars(num)
    case Lambda(_clauses) =>
      // a fun expression does not introduce new variables to the containing scope
      // X = 1, fun () -> X = 2 end, X. % 1
      Set.empty
    case UnOp(op, arg) =>
      exprVars(arg)
    case BinOp(op, arg1, arg2) =>
      exprVars(arg1) ++ exprVars(arg2)
    case Binary(elems) =>
      elems.flatMap(binaryElemVars).toSet
    case Catch(e) =>
      exprVars(e)
    case TryCatchExpr(tryBody, catchClauses, after) =>
      Set.empty
    case TryOfCatchExpr(tryBody, tryClauses, catchClauses, after) =>
      Set.empty
    case Receive(clauses) =>
      clausesVars(clauses)
    case ReceiveWithTimeout(List(), _, timeoutBody) =>
      bodyVars(timeoutBody)
    case ReceiveWithTimeout(clauses, timeout, timeoutBody) =>
      clausesVars(clauses) intersect bodyVars(timeoutBody)
    case LComprehension(_, _) =>
      Set.empty
    case BComprehension(_, _) =>
      Set.empty
    case MComprehension(_, _, _) =>
      Set.empty
    case RecordCreate(_, fields) =>
      fields.flatMap(fieldVars).toSet
    case RecordUpdate(e, recName, fields) =>
      exprVars(e) ++ fields.flatMap(fieldVars)
    case RecordSelect(e, _, _) =>
      exprVars(e)
    case RecordIndex(_, _) =>
      Set.empty
    case MapCreate(kvs) =>
      kvs.flatMap(kv => List(kv._1, kv._2)).flatMap(exprVars).toSet
    case MapUpdate(m, kvs) =>
      exprVars(m) ++ kvs.flatMap(kv => List(kv._1, kv._2)).flatMap(exprVars)
  }

  private def fieldVars(recordField: RecordField): Set[String] =
    exprVars(recordField.value)

  def clausesVars(clauses: List[Clause]): Set[String] =
    clauses.map(clauseVars).reduce(_ intersect _)

  def clausesAndBlockVars(clauses: List[Clause], body: Body): Set[String] =
    (bodyVars(body) :: clauses.map(clauseVars)).reduce(_ intersect _)

  private def clauseVars(clause: Clause): Set[String] =
    clause.pats.flatMap(patVars).toSet ++ bodyVars(clause.body)

  def clausePatVars(clause: Clause): Set[String] =
    clause.pats.flatMap(patVars).toSet

}

final class Vars(pipelineContext: PipelineContext) {

  private lazy val module = pipelineContext.module
  private lazy val util = pipelineContext.util

  private def patVarsL(pat: Pat): List[String] =
    pat match {
      case PatWild() =>
        List.empty
      case PatMatch(pat, arg) =>
        patVarsL(pat) ++ patVarsL(arg)
      case PatTuple(elems) =>
        elems.flatMap(patVarsL)
      case PatString() =>
        List.empty
      case PatNil() =>
        List.empty
      case PatCons(h, t) =>
        patVarsL(h) ++ patVarsL(t)
      case PatNumber() | PatInt() =>
        List.empty
      case PatAtom(_) =>
        List.empty
      case PatVar(n) =>
        List(n)
      case PatUnOp(_, arg) =>
        patVarsL(arg)
      case PatBinOp(_, arg1, arg2) =>
        patVarsL(arg1) ++ patVarsL(arg2)
      case PatBinary(elems) =>
        elems.flatMap(binaryElemVarsL)
      case PatRecord(recName, fields, gen) =>
        val fieldsVars = fields.flatMap(f => patVarsL(f.pat))
        gen match {
          case Some(genPat) =>
            val matchedFields = fields.map(_.name)
            val genFields =
              util
                .getRecord(module, recName)
                .map(_.fields.keySet -- matchedFields)
                .getOrElse(Set.empty)
            fieldsVars ++ List.fill(genFields.size)(patVarsL(genPat)).flatten
          case None => fieldsVars
        }
      case PatRecordIndex(_, _) =>
        List.empty
      case PatMap(kvs) =>
        kvs.flatMap(kv => patVarsL(kv._2))
    }

  private def binaryElemVarsL(elem: PatBinaryElem): List[String] =
    patVarsL(elem.pat)

  def clausePatVarsL(clause: Clause): List[String] =
    clause.pats.flatMap(patVarsL)
}
