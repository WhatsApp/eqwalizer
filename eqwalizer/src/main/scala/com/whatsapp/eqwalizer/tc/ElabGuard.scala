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

final class ElabGuard(pipelineContext: PipelineContext) {
  private lazy val module = pipelineContext.module
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype
  private lazy val occurrence = pipelineContext.occurrence
  private lazy val typeInfo = pipelineContext.typeInfo
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo

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
    case "is_map"       => MapType(Map(), AnyType, AnyType)
    case "is_tuple"     => AnyTupleType
  }

  private val elabPredicateType21: PartialFunction[(String, Test), Type] = {
    case ("is_map_key", _) =>
      MapType(Map(), AnyType, AnyType)
    case ("element", _) =>
      AnyTupleType
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

  def elabTestT(test: Test, upper: Type, env: Env): Env = {
    test match {
      case TestVar(v) =>
        val testType = env.get(v) match {
          case Some(vt) =>
            narrow.meet(vt, upper)
          case None => upper
        }
        typeInfo.add(test.pos, testType)
        env + (v -> testType)
      case TestCall(Id("is_map_key", 2), List(kTest: TestVar, map: TestMapCreate)) if isLiteralKeysMap(map) =>
        val kType = getKeyType(map)
        elabTestT(kTest, kType, env)
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
      case TestBinOp("and" | "andalso", arg1, arg2) if upper == trueType =>
        val env1 = elabTestT(arg1, trueType, env)
        elabTestT(arg2, trueType, env1)
      case TestBinOp("orelse", arg1, arg2) if upper == trueType =>
        val envTrue = elabTestT(arg1, trueType, env)
        val envFalse = elabTestT(arg1, falseType, env)
        val envFalse2 = elabTestT(arg2, trueType, envFalse)
        subtype.joinEnvs(List(envTrue, envFalse2))
      case TestBinOp("or", arg1, arg2) if upper == trueType =>
        val envTrue = elabTestT(arg1, trueType, env)
        val envTrue2 = elabTestT(arg2, booleanType, envTrue)
        val envFalse = elabTestT(arg1, booleanType, env)
        val envFalse2 = elabTestT(arg2, trueType, envFalse)
        subtype.joinEnvs(List(envTrue2, envFalse2))
      case TestAtom(_) =>
        env
      case TestNumber(_) =>
        env
      case TestTuple(elems) =>
        var envAcc: Env = env
        for (elem <- elems) {
          val elemEnv = elabTestT(elem, AnyType, envAcc)
          envAcc = elemEnv
        }
        envAcc
      case TestString() =>
        env
      case TestNil() =>
        env
      case TestCons(head, tail) =>
        val env1 = elabTestT(head, AnyType, env)
        val env2 = elabTestT(tail, AnyType, env1)
        env2
      case TestMapCreate(kvs) =>
        var envAcc: Env = env
        for ((k, v) <- kvs) {
          val kEnv = elabTestT(k, AnyType, envAcc)
          val vEnv = elabTestT(v, AnyType, kEnv)
          envAcc = vEnv
        }
        envAcc
      case TestCall(_, args) =>
        var envAcc: Env = env
        for (arg <- args) {
          val argEnv = elabTestT(arg, AnyType, envAcc)
          envAcc = argEnv
        }
        env
      case unOp: TestUnOp =>
        elabUnOp(unOp, upper, env)
      case binOp: TestBinOp =>
        elabBinOp(binOp, upper, env)
      case TestBinaryLit() =>
        env
      case TestRecordIndex(_, _) =>
        env
      case TestRecordSelect(rec, recName, _) =>
        val ty = RecordType(recName)(module)
        elabTestT(rec, ty, env)
      case TestRecordCreate(recName, fields) =>
        val recDecl =
          util.getRecord(module, recName) match {
            case Some(recDecl) => recDecl
            case None =>
              diagnosticsInfo.add(UnboundRecord(test.pos, recName))
              return env
          }
        val namedFields = fields.collect { case f: TestRecordFieldNamed => f }
        val optGenField = fields.collectFirst { case f: TestRecordFieldGen => f }
        val genFields = recDecl.fMap.keySet -- namedFields.map(_.name)
        val undefinedFields =
          optGenField match {
            case Some(_) => Set.empty
            case None    => genFields
          }
        for (uField <- undefinedFields) {
          val fieldDecl = recDecl.fMap(uField)
          if (fieldDecl.defaultValue.isEmpty && !subtype.subType(undefined, fieldDecl.tp)) {
            diagnosticsInfo.add(UndefinedField(test.pos, recName, uField))
          }
        }
        var envAcc = env
        for (field <- namedFields) {
          val fieldDecl = recDecl.fMap(field.name)
          envAcc = elabTestT(field.value, fieldDecl.tp, envAcc)
        }
        optGenField match {
          case Some(field) =>
            for (genFieldName <- genFields) {
              val fieldDecl = recDecl.fMap(genFieldName)
              envAcc = elabTestT(field.value, fieldDecl.tp, envAcc)
            }
          case None => ()
        }
        envAcc
      case TestMapUpdate(map, _) =>
        val ty = MapType(Map(), AnyType, AnyType)
        elabTestT(map, ty, env)
    }
  }

  private def elabUnOp(unOp: TestUnOp, upper: Type, env: Env): Env = {
    val TestUnOp(op, arg) = unOp
    op match {
      case "not" if upper == trueType  => elabTestT(arg, falseType, env)
      case "not" if upper == falseType => elabTestT(arg, trueType, env)
      case "not"                       => elabTestT(arg, booleanType, env)
      case "bnot" | "+" | "-" =>
        elabTestT(arg, NumberType, env)
      case _ =>
        throw UnhandledOp(unOp.pos, op)
    }
  }

  private object NumTest {
    def unapply(test: Test): Boolean = test match {
      case TestNumber(_)                => true
      case TestUnOp("+", TestNumber(_)) => true
      case TestUnOp("-", TestNumber(_)) => true
      case _                            => false
    }
  }

  private def elabComparison(binOp: TestBinOp, upper: Type, env: Env): Env =
    binOp match {
      case TestBinOp("=:=" | "==", TestVar(v), NumTest()) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, NumberType))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", NumTest(), TestVar(v)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, NumberType))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestVar(v), TestString()) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, stringType))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestString(), TestVar(v)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, stringType))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestVar(v), TestAtom(a)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestAtom(a), TestVar(v)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.meet(ty, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestCall(Id("element", 2), List(TestNumber(Some(i)), TestVar(v))), TestAtom(a))
          if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.filterTupleType(ty, i, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp("=:=" | "==", TestAtom(a), TestCall(Id("element", 2), List(TestNumber(Some(i)), TestVar(v))))
          if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> narrow.filterTupleType(ty, i, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp("=/=" | "/=", TestVar(v), TestAtom(a)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> occurrence.remove(ty, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp("=/=" | "/=", TestAtom(a), TestVar(v)) if upper == trueType =>
        env.get(v) match {
          case Some(ty) =>
            env + (v -> occurrence.remove(ty, AtomLitType(a)))
          case None =>
            env
        }
      case TestBinOp(_, arg1, arg2) =>
        val env1 = elabTestT(arg1, AnyType, env)
        elabTestT(arg2, AnyType, env1)
    }

  private def elabBinOp(binOp: TestBinOp, upper: Type, env: Env): Env = {
    val TestBinOp(op, arg1, arg2) = binOp
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
        val env1 = elabTestT(arg1, NumberType, env)
        elabTestT(arg2, NumberType, env1)
      case "or" | "xor" | "and" =>
        val env1 = elabTestT(arg1, booleanType, env)
        elabTestT(arg2, booleanType, env1)
      case ">=" | ">" | "=<" | "<" | "/=" | "=/=" | "==" | "=:=" =>
        elabComparison(binOp, upper, env)
      case "andalso" =>
        val env1 = elabTestT(arg1, booleanType, env)
        elabTestT(arg2, upper, env1)
      case "orelse" =>
        val env1 = elabTestT(arg1, booleanType, env)
        elabTestT(arg2, upper, env1)
      case _ =>
        throw new IllegalStateException(s"unexpected $op")
    }
  }

  private def isLiteralKeysMap(map: TestMapCreate): Boolean =
    map.kvs.forall((k, _) => k.isInstanceOf[TestAtom])

  private def getKeyType(map: TestMapCreate): Type = {
    val atoms = map.kvs.map { case (k, _) =>
      val TestAtom(a) = k: @unchecked
      AtomLitType(a)
    }
    UnionType(atoms.toSet)
  }
}
