/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Forms.FunDecl

class Patch(erroneousExpr: Expr) {
  def patchFun(fun: FunDecl): FunDecl =
    fun.copy(clauses = fun.clauses.map(patchClause))(fun.pos)

  private def dynamicCast(expr: Expr): Expr =
    RemoteCall(RemoteId("eqwalizer", "dynamic_cast", 1), List(expr))(expr.pos)

  private def patchClause(clause: Clause): Clause =
    clause.copy(body = patchBody(clause.body))(clause.pos)

  private def patchBody(body: Body): Body =
    body.copy(exprs = body.exprs.map(patchExpr))

  private def patchExpr(e: Expr): Expr =
    if (erroneousExpr.pos == e.pos && erroneousExpr == e)
      dynamicCast(e)
    else
      e match {
        case Block(body) =>
          Block(patchBody(body))(e.pos)
        case Match(pat, expr) =>
          Match(pat, patchExpr(expr))(e.pos)
        case Tuple(elems) =>
          Tuple(elems.map(patchExpr))(e.pos)
        case Cons(h, t) =>
          Cons(patchExpr(h), patchExpr(t))(e.pos)
        case Case(expr, clauses) =>
          Case(patchExpr(expr), clauses.map(patchClause))(e.pos)
        case If(clauses) =>
          If(clauses.map(patchClause))(e.pos)
        case LocalCall(id, args) =>
          LocalCall(id, args.map(patchExpr))(e.pos)
        case DynCall(f, args) =>
          DynCall(patchExpr(f), args.map(patchExpr))(e.pos)
        case RemoteCall(id, args) =>
          RemoteCall(id, args.map(patchExpr))(e.pos)
        case l @ Lambda(clauses) =>
          Lambda(clauses.map(patchClause))(e.pos, l.name)
        case UnOp(op, arg) =>
          UnOp(op, patchExpr(arg))(e.pos)
        case BinOp(op, arg1, arg2) =>
          BinOp(op, patchExpr(arg1), patchExpr(arg2))(e.pos)
        case LComprehension(template, qualifiers) =>
          LComprehension(patchExpr(template), qualifiers.map(patchQualifier))(e.pos)
        case BComprehension(template, qualifiers) =>
          BComprehension(patchExpr(template), qualifiers.map(patchQualifier))(e.pos)
        case MComprehension(kTemplate, vTemplate, qualifiers) =>
          MComprehension(patchExpr(kTemplate), patchExpr(vTemplate), qualifiers.map(patchQualifier))(e.pos)
        case Binary(elems) =>
          Binary(elems.map(patchBinaryElem))(e.pos)
        case Catch(expr) =>
          Catch(patchExpr(expr))(e.pos)
        case TryCatchExpr(tryBody, catchClauses, afterBody) =>
          TryCatchExpr(patchBody(tryBody), catchClauses.map(patchClause), afterBody.map(patchBody))(e.pos)
        case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
          TryOfCatchExpr(
            patchBody(tryBody),
            tryClauses.map(patchClause),
            catchClauses.map(patchClause),
            afterBody.map(patchBody),
          )(e.pos)
        case Receive(clauses) =>
          Receive(clauses.map(patchClause))(e.pos)
        case ReceiveWithTimeout(clauses, timeout, timeoutBody) =>
          ReceiveWithTimeout(clauses.map(patchClause), patchExpr(timeout), patchBody(timeoutBody))(e.pos)
        case RecordCreate(recName, fields) =>
          RecordCreate(recName, fields.map(patchRecordField))(e.pos)
        case RecordUpdate(expr, recName, fields) =>
          RecordUpdate(patchExpr(expr), recName, fields.map(patchRecordFieldNamed))(e.pos)
        case RecordSelect(expr, recName, fieldName) =>
          RecordSelect(patchExpr(expr), recName, fieldName)(e.pos)
        case MapCreate(kvs) =>
          MapCreate(kvs.map(patchKV))(e.pos)
        case MapUpdate(map, kvs) =>
          MapUpdate(patchExpr(map), kvs.map(patchKV))(e.pos)
        case MaybeMatch(pat, exp) =>
          MaybeMatch(pat, patchExpr(exp))(e.pos)
        case Maybe(body) =>
          Maybe(patchBody(body))(e.pos)
        case MaybeElse(body, elseClauses) =>
          MaybeElse(patchBody(body), elseClauses.map(patchClause))(e.pos)
        // The erroneous expr may be a variable introduced by eta-expansion of a functional argument,
        // in which case e == erroneousExpr will never match and the expr will not be patched.
        // The case below is a good enough heuristic to detect such cases.
        case _: LocalFun | _: RemoteFun if erroneousExpr.pos == e.pos && erroneousExpr.isInstanceOf[Var] =>
          dynamicCast(e)
        case _: Var | _: AtomLit | _: IntLit | FloatLit() | StringLit(_) | NilLit() | _: LocalFun | _: RemoteFun |
            _: DynRemoteFun | _: DynRemoteFunArity | _: RecordIndex =>
          e
      }

  private def patchQualifier(qualifier: Qualifier): Qualifier =
    qualifier match {
      case LGenerate(pat, expr) =>
        LGenerate(pat, patchExpr(expr))
      case BGenerate(pat, expr) =>
        BGenerate(pat, patchExpr(expr))
      case MGenerate(kPat, vPat, expr) =>
        MGenerate(kPat, vPat, patchExpr(expr))
      case Filter(expr) =>
        Filter(patchExpr(expr))
    }

  private def patchBinaryElem(binaryElem: BinaryElem): BinaryElem = {
    val BinaryElem(expr, size, specifier) = binaryElem
    BinaryElem(patchExpr(expr), size.map(patchExpr), specifier)(binaryElem.pos)
  }

  private def patchRecordField(recordField: RecordField): RecordField =
    recordField match {
      case named: RecordFieldNamed =>
        patchRecordFieldNamed(named)
      case RecordFieldGen(value) =>
        RecordFieldGen(patchExpr(value))
    }

  private def patchRecordFieldNamed(recordFieldNamed: RecordFieldNamed): RecordFieldNamed = {
    val RecordFieldNamed(name, value) = recordFieldNamed
    RecordFieldNamed(name, patchExpr(value))
  }

  private def patchKV(kv: (Expr, Expr)): (Expr, Expr) = {
    val (k, v) = kv
    (patchExpr(k), patchExpr(v))
  }
}
