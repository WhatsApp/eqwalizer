/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.BinarySpecifiers.Specifier
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Pats.Pat

object Exprs {
  case class Body(exprs: List[Expr])

  sealed trait Expr { val pos: Pos }
  case class Var(n: String)(val pos: Pos) extends Expr
  case class AtomLit(s: String)(val pos: Pos) extends Expr

  case class IntLit(value: BigInt)(val pos: Pos) extends Expr
  case class FloatLit()(val pos: Pos) extends Expr

  case class Block(body: Body)(val pos: Pos) extends Expr
  case class Match(pat: Pat, expr: Expr)(val pos: Pos) extends Expr

  case class Tuple(elems: List[Expr])(val pos: Pos) extends Expr

  case class StringLit(empty: Boolean)(val pos: Pos) extends Expr

  case class NilLit()(val pos: Pos) extends Expr
  case class Cons(h: Expr, t: Expr)(val pos: Pos) extends Expr

  case class Case(expr: Expr, clauses: List[Clause])(val pos: Pos) extends Expr
  case class If(clauses: List[Clause])(val pos: Pos) extends Expr

  case class LocalCall(id: Id, args: List[Expr])(val pos: Pos) extends Expr
  case class DynCall(f: Expr, args: List[Expr])(val pos: Pos) extends Expr

  case class RemoteCall(id: RemoteId, args: List[Expr])(val pos: Pos) extends Expr
  case class LocalFun(id: Id)(val pos: Pos) extends Expr
  case class RemoteFun(id: RemoteId)(val pos: Pos) extends Expr
  case class DynRemoteFun(mod: Expr, name: Expr)(val pos: Pos) extends Expr
  case class DynRemoteFunArity(mod: Expr, name: Expr, arity: Expr)(val pos: Pos) extends Expr

  case class Lambda(clauses: List[Clause])(val pos: Pos, val name: Option[String]) extends Expr

  case class UnOp(op: String, arg: Expr)(val pos: Pos) extends Expr
  case class BinOp(op: String, arg1: Expr, arg2: Expr)(val pos: Pos) extends Expr

  case class LComprehension(template: Expr, qualifiers: List[Qualifier])(val pos: Pos) extends Expr
  case class BComprehension(template: Expr, qualifiers: List[Qualifier])(val pos: Pos) extends Expr
  case class Binary(elems: List[BinaryElem])(val pos: Pos) extends Expr
  case class Catch(expr: Expr)(val pos: Pos) extends Expr
  case class TryCatchExpr(tryBody: Body, catchClauses: List[Clause], afterBody: Option[Body])(val pos: Pos) extends Expr
  case class TryOfCatchExpr(
      tryBody: Body,
      tryClauses: List[Clause],
      catchClauses: List[Clause],
      afterBody: Option[Body],
  )(val pos: Pos)
      extends Expr
  case class Receive(clauses: List[Clause])(val pos: Pos) extends Expr
  case class ReceiveWithTimeout(clauses: List[Clause], timeout: Expr, timeoutBody: Body)(val pos: Pos) extends Expr

  case class RecordCreate(recName: String, fields: List[RecordField])(val pos: Pos) extends Expr
  case class RecordUpdate(expr: Expr, recName: String, fields: List[RecordFieldNamed])(val pos: Pos) extends Expr
  case class RecordSelect(expr: Expr, recName: String, fieldName: String)(val pos: Pos) extends Expr
  case class RecordIndex(recName: String, fieldName: String)(val pos: Pos) extends Expr

  case class MapCreate(kvs: List[(Expr, Expr)])(val pos: Pos) extends Expr
  case class ReqMapUpdate(map: Expr, kvs: List[(String, Expr)])(val pos: Pos) extends Expr
  case class GenMapUpdate(map: Expr, kvs: List[(Expr, Expr)])(val pos: Pos, val approximated: Boolean) extends Expr

  case class Clause(pats: List[Pat], guards: List[Guard], body: Body)(val pos: Pos)
  case class BinaryElem(expr: Expr, size: Option[Expr], specifier: Specifier)(val pos: Pos)
  sealed trait RecordField {
    val value: Expr
  }
  case class RecordFieldNamed(name: String, value: Expr) extends RecordField
  case class RecordFieldGen(value: Expr) extends RecordField

  sealed trait Qualifier
  sealed trait Generator extends Qualifier
  case class LGenerate(pat: Pat, expr: Expr) extends Generator
  case class BGenerate(pat: Pat, expr: Expr) extends Generator
  case class Filter(expr: Expr) extends Qualifier
}
