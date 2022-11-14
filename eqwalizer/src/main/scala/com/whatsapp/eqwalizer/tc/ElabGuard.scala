/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{UnboundRecord, UndefinedField, UnhandledOp}

import scala.collection.mutable.ListBuffer

final class ElabGuard(pipelineContext: PipelineContext) {
  private lazy val module = pipelineContext.module
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype
  private lazy val occurrence = pipelineContext.occurrence

  private val elabPredicateType1: PartialFunction[String, Type] = {
    case "is_atom"      => AtomType
    case "is_binary"    => BinaryType
    case "is_bitstring" => BinaryType
    case "is_boolean"   => UnionType(Set(falseType, trueType))
    case "is_float"     => floatType
    case "is_function"  => AnyFunType
    case "is_integer"   => NumberType
    case "is_list"      => ListType(AnyType)
    case "is_number"    => NumberType
    case "is_pid"       => PidType
    case "is_port"      => PortType
    case "is_reference" => ReferenceType
    case "is_map"       => DictMap(AnyType, AnyType)
    case "is_tuple"     => AnyTupleType
  }

  private val elabPredicateType21: PartialFunction[(String, Test), Type] = { case ("is_map_key", _) =>
    DictMap(AnyType, AnyType)
  }

  private val elabPredicateType22: PartialFunction[(String, Test), Type] = {
    case ("is_record", TestAtom(recName)) =>
      RecordType(recName)(module)
    case ("is_function", TestNumber(Some(arity))) =>
      FunType(Nil, List.fill(arity.intValue)(AnyType), AnyType)
    case ("is_function", _) =>
      AnyFunType
  }

  def elabGuards(guards: List[Guard], env: Env): Env =
    if (guards.isEmpty) env
    else subtype.joinEnvs(guards.map(elabGuard(_, env)))

  private def elabGuard(guard: Guard, env: Env): Env = {
    var envAcc = env
    guard.tests.foreach { test =>
      envAcc = elabTestT(test, trueType, envAcc)
    }
    envAcc
  }

  private def elabTest(test: Test, env: Env): (Type, Env) = {
    test match {
      case TestVar(v) =>
        // safe because we assume no unbound vars
        val ty = env.getOrElse(
          v,
          // $COVERAGE-OFF$
          AnyType,
          // $COVERAGE-ON$
        )
        (ty, env)
      case TestAtom(lit) =>
        (AtomLitType(lit), env)
      case TestNumber(_) =>
        (NumberType, env)
      case TestTuple(elems) =>
        var envAcc: Env = env
        val elemTys = ListBuffer[Type]()
        for (elem <- elems) {
          val (elemT, elemEnv) = elabTest(elem, envAcc)
          elemTys += elemT
          envAcc = elemEnv
        }
        (TupleType(elemTys.toList), envAcc)
      case TestString() =>
        (stringType, env)
      case TestNil() =>
        (NilType, env)
      case TestCons(head, tail) =>
        val (headTy, env1) = elabTest(head, env)
        val (tailTy, env2) = elabTest(tail, env1)
        if (!subtype.subType(tailTy, ListType(AnyType))) {
          (ListType(headTy), env2)
        } else {
          val resType = narrow.asListType(tailTy) match {
            case Some(ListType(t)) => ListType(subtype.join(headTy, t))
            case None              => headTy
          }
          (resType, env2)
        }
      case TestMapCreate(kvs) =>
        var envAcc: Env = env
        val kvTys = ListBuffer[(Type, Type)]()
        for ((k, v) <- kvs) {
          val (kTy, kEnv) = elabTest(k, envAcc)
          val (vTy, vEnv) = elabTest(k, kEnv)
          kvTys += kTy -> vTy
          envAcc = vEnv
        }
        val isShape = kvs.forall(_._1.isInstanceOf[TestAtom])
        if (isShape) {
          val props = kvTys.collect { case (AtomLitType(k), v) => ReqProp(k, v) }.toList
          (ShapeMap(props), envAcc)
        } else {
          val domain = kvTys.map(_._1).reduce(subtype.join)
          val codomain = kvTys.map(_._2).reduce(subtype.join)
          (DictMap(domain, codomain), envAcc)
        }
      case TestCall(id, args) =>
        var envAcc: Env = env
        val argTys = ListBuffer[Type]()
        for (arg <- args) {
          val (argTy, argEnv) = elabTest(arg, envAcc)
          argTys += argTy
          envAcc = argEnv
        }
        // approx: opacity checking ignores overloaded functions,
        // elabApplyCustom functions, and generic functions
        // because those functions currently require `Expr`s, not tests
        val resTy = util.getFunType("erlang", id) match {
          case Some(ft @ FunType(Nil, _argTys, resTy)) =>
            resTy
          case _ =>
            // $COVERAGE-OFF$
            AnyType
          // $COVERAGE-ON$
        }
        (resTy, env)
      case unOp: TestUnOp =>
        elabUnOp(unOp, env)
      case binOp: TestBinOp =>
        elabBinOp(binOp, env)
      case TestBinaryLit() =>
        (BinaryType, env)
      case TestRecordIndex(_, _) =>
        (NumberType, env)
      case TestRecordSelect(rec, recName, _) =>
        val ty = RecordType(recName)(module)
        (ty, elabTestT(rec, ty, env))
      case TestRecordCreate(recName, fields) =>
        val recDecl = util.getRecord(module, recName).getOrElse(throw UnboundRecord(test.pos, recName))
        val namedFields = fields.collect { case f: TestRecordFieldNamed => f }
        val optGenField = fields.collectFirst { case f: TestRecordFieldGen => f }
        val genFields = recDecl.fields.keySet -- namedFields.map(_.name)
        val undefinedFields =
          optGenField match {
            case Some(_) => Set.empty
            case None    => genFields
          }
        for (uField <- undefinedFields) {
          val fieldDecl = recDecl.fields(uField)
          if (fieldDecl.defaultValue.isEmpty && !subtype.subType(undefined, fieldDecl.tp)) {
            throw UndefinedField(test.pos, recName, uField)
          }
        }
        var envAcc = env
        for (field <- namedFields) {
          val fieldDecl = recDecl.fields(field.name)
          envAcc = elabTestT(field.value, fieldDecl.tp, envAcc)
        }
        optGenField match {
          case Some(field) =>
            for (genFieldName <- genFields) {
              val fieldDecl = recDecl.fields(genFieldName)
              envAcc = elabTestT(field.value, fieldDecl.tp, envAcc)
            }
          case None => ()
        }
        (RecordType(recName)(module), envAcc)
      case TestReqMapUpdate(map, _) =>
        val ty = DictMap(AnyType, AnyType)
        (ty, elabTestT(map, ty, env))
      case TestGenMapUpdate(map, _) =>
        val ty = DictMap(AnyType, AnyType)
        (ty, elabTestT(map, ty, env))
    }
  }

  def elabTestT(test: Test, upper: Type, env: Env): Env =
    test match {
      case TestVar(v) =>
        val testType = env.get(v) match {
          case Some(vt) =>
            narrow.meet(vt, upper)
          case None => upper
        }
        env + (v -> testType)
      case TestCall(Id(pred, 1), List(arg)) if upper == trueType && elabPredicateType1.isDefinedAt(pred) =>
        elabTestT(arg, elabPredicateType1(pred), env)
      case TestCall(Id(pred, 2), List(arg1, arg2))
          if upper == trueType && elabPredicateType22.isDefinedAt((pred, arg2)) =>
        elabTestT(arg1, elabPredicateType22(pred, arg2), env)
      case TestCall(Id(pred, 2), List(arg1, arg2))
          if upper == trueType && elabPredicateType21.isDefinedAt((pred, arg1)) =>
        elabTestT(arg2, elabPredicateType21(pred, arg1), env)
      case TestCall(Id(pred, 3), List(arg1, arg2, _))
          if upper == trueType && elabPredicateType22.isDefinedAt((pred, arg2)) =>
        elabTestT(arg1, elabPredicateType22(pred, arg2), env)
      case TestBinOp("and", arg1, arg2) =>
        val env1 = elabTestT(arg1, AtomLitType("true"), env)
        val env2 = elabTestT(arg2, upper, env1)
        env2
      case TestBinOp("andalso", arg1, arg2) =>
        val env1 = elabTestT(arg1, AtomLitType("true"), env)
        val env2 = elabTestT(arg2, upper, env1)
        env2
      case TestBinOp("orelse", arg1, _) =>
        val env1 = elabTestT(arg1, booleanType, env)
        env1
      case TestBinOp("or", arg1, arg2) =>
        val env1 = elabTestT(arg1, booleanType, env)
        // "or" is not short-circuiting
        val env2 = elabTestT(arg2, booleanType, env1)
        env2
      case _ =>
        elabTest(test, env)._2
    }

  def elabUnOp(unOp: TestUnOp, env: Env): (Type, Env) = {
    val TestUnOp(op, arg) = unOp
    op match {
      case "not" =>
        (booleanType, elabTestT(arg, booleanType, env))
      case "bnot" | "+" | "-" =>
        (NumberType, elabTestT(arg, NumberType, env))
      // $COVERAGE-OFF$
      case _ => throw UnhandledOp(unOp.pos, op)
      // $COVERAGE-ON$
    }
  }

  private object NumTest {
    def unapply(test: Test): Option[BigInt] = test match {
      case TestNumber(nOpt)                   => nOpt
      case TestUnOp("+", TestNumber(nOpt))    => nOpt
      case TestUnOp("-", TestNumber(Some(n))) => Some(-n)
      case _                                  => None
    }
  }

  private def elabComparison(binOp: TestBinOp, env: Env): Env =
    binOp match {
      case TestBinOp("=:=" | "==", TestVar(v), NumTest(n)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, NumberType))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=:=" | "==", NumTest(n), TestVar(v)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, NumberType))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=:=" | "==", TestVar(v), TestString()) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, stringType))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=:=" | "==", TestString(), TestVar(v)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, stringType))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=:=" | "==", TestVar(v), TestAtom(a)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, AtomLitType(a)))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=:=" | "==", TestAtom(a), TestVar(v)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, AtomLitType(a)))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=/=" | "/=", TestVar(v), TestAtom(a)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> occurrence.remove(ty, AtomLitType(a)))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp("=/=" | "/=", TestAtom(a), TestVar(v)) =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> occurrence.remove(ty, AtomLitType(a)))
          // $COVERAGE-OFF$
          case None =>
            env
          // $COVERAGE-ON$
        }
      case TestBinOp(op, arg1, arg2) =>
        val (arg1Ty, env1) = elabTest(arg1, env)
        val (arg2Ty, env2) = elabTest(arg2, env1)
        env2
    }

  private def elabBinOp(binOp: TestBinOp, env: Env): (Type, Env) = {
    val TestBinOp(op, arg1, arg2) = binOp
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
        val env1 = elabTestT(arg1, NumberType, env)
        val env2 = elabTestT(arg2, NumberType, env1)
        (NumberType, env2)
      case "or" | "xor" | "and" =>
        val env1 = elabTestT(arg1, booleanType, env)
        val env2 = elabTestT(arg2, booleanType, env1)
        (booleanType, env2)
      case ">=" | ">" | "=<" | "<" | "/=" | "=/=" | "==" | "=:=" =>
        (booleanType, elabComparison(binOp, env))
      case _ =>
        // $COVERAGE-OFF$
        throw new IllegalStateException(s"unexpected $op")
      // $COVERAGE-ON$
    }
  }
}
