/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{BinarySpecifiers, Filters, Pats, RemoteId, Vars}
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

final class Elab(pipelineContext: PipelineContext) {
  private lazy val module = pipelineContext.module
  private lazy val check = pipelineContext.check
  private lazy val elabPat = pipelineContext.elabPat
  private lazy val elabGuard = pipelineContext.elabGuard
  private lazy val elabApply = pipelineContext.elabApply
  private lazy val elabApplyCustom = pipelineContext.elabApplyCustom
  private lazy val elabApplyOverloaded = pipelineContext.elabApplyOverloaded
  private lazy val subtype = pipelineContext.subtype
  private lazy val util = pipelineContext.util
  private lazy val narrow = pipelineContext.narrow
  private lazy val occurrence = pipelineContext.occurrence
  private lazy val typeInfo = pipelineContext.typeInfo
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def elabBody(body: Body, env: Env): (Type, Env) = {
    val exprs = body.exprs
    var (elabType, envAcc) = elabExpr(exprs.head, env)
    for (expr <- exprs.tail) {
      val (t1, env1) = elabExpr(expr, envAcc)
      elabType = t1
      envAcc = env1
    }
    (elabType, envAcc)
  }

  def elabClause(clause: Clause, argTys: List[Type], env0: Env, exportedVars: Set[String]): (Type, Env) = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(env0, patVars)
    // see D29637051 for why we elabGuard twice
    val env2 = elabGuard.elabGuards(clause.guards, env1)
    // Erase type info from first elaboration
    typeInfo.clear(clause.pos)
    val (_, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    val env4 = elabGuard.elabGuards(clause.guards, env3)
    val (eType, env5) = elabBody(clause.body, env4)
    val env6 = util.exitScope(env0, env5, exportedVars)
    (eType, env6)
  }

  def elabExprs(exprs: List[Expr], env: Env): (List[Type], Env) = {
    var envAcc = env
    val tys = exprs.map { expr =>
      val (ty, env1) = elabExpr(expr, envAcc)
      envAcc = env1
      ty
    }
    (tys, envAcc)
  }

  private def elabMaybeBody(body: Body, env: Env): (Type, Env) = {
    var envAcc = env
    var tyAcc: Type = NoneType
    var lastTy: Type = NoneType
    val exprs = body.exprs
    for (expr <- exprs) {
      expr match {
        case MaybeMatch(mPat, mExp) =>
          val (mType, env1) = elabExpr(mExp, envAcc)
          val (patTy, env2) = elabPat.elabPat(mPat, mType, env1)
          tyAcc = subtype.join(tyAcc, mType)
          lastTy = patTy
          envAcc = env2
        case _ =>
          val (expTy, env1) = elabExpr(expr, envAcc)
          lastTy = expTy
          envAcc = env1
      }
    }
    (subtype.join(tyAcc, lastTy), env)
  }

  def elabExpr(expr: Expr, env: Env): (Type, Env) =
    expr match {
      case Var(v) =>
        val ty = env.getOrElse(v, throw UnboundVar(expr.pos, v))
        typeInfo.add(expr.pos, ty)
        (ty, env)
      case AtomLit(a) =>
        (AtomLitType(a), env)
      case FloatLit() =>
        (NumberType, env)
      case IntLit(_) =>
        (NumberType, env)
      case Tuple(elems) =>
        var envAcc = env
        val elemTypes = elems.map { elem =>
          val (eType, env1) = elabExpr(elem, envAcc)
          envAcc = env1
          eType
        }
        (TupleType(elemTypes), envAcc)
      case StringLit(empty) =>
        val litType = if (empty) NilType else stringType
        (litType, env)
      case NilLit() =>
        (NilType, env)
      case Cons(head, NilLit()) =>
        val (headT, env1) = elabExpr(head, env)
        val resType = subtype.join(util.flattenUnions(headT).map(ListType).toSet)
        (resType, env1)
      case Cons(head, tail) =>
        val (headT, env1) = elabExpr(head, env)
        val (tailT, env2) = elabExpr(tail, env1)
        if (!subtype.subType(tailT, ListType(AnyType))) {
          throw ExpectedSubtype(tail.pos, tail, expected = ListType(AnyType), got = tailT)
        } else {
          val resType = narrow.asListType(tailT) match {
            case Some(ListType(t)) => ListType(subtype.join(headT, t))
            case None              => headT
          }
          (resType, env2)
        }
      case LocalCall(id, args) =>
        val funId = util.globalFunId(module, id)
        if (elabApplyCustom.isCustom(funId)) {
          elabApplyCustom.elabCustom(funId, args, env, expr.pos)
        } else if (elabApplyOverloaded.isOverloadedFun(funId)) {
          elabApplyOverloaded.elabOverloaded(expr, funId, args, env)
        } else
          util.getFunType(module, id) match {
            case Some(ft) =>
              val (argTys, env1) = elabExprs(args, env)
              var resTy = elabApply.elabApply(check.freshen(ft), args, argTys, env1)
              if (CustomReturn.isCustomReturn(funId))
                resTy = CustomReturn.customizeResultType(funId, args, resTy)
              (resTy, env1)
            case None =>
              throw UnboundVar(expr.pos, id.toString)
          }
      case DynCall(l: Lambda, args) =>
        val arity = l.clauses.head.pats.size
        if (arity != args.size) throw LambdaArityMismatch(l.pos, l, lambdaArity = arity, argsArity = args.size)
        val (argTys, env1) = elabExprs(args, env)
        l.name match {
          case Some(name) if pipelineCtx.gradualTyping =>
            val funType = FunType(Nil, List.fill(argTys.size)(DynamicType), DynamicType)
            val env2 = env.updated(name, funType)
            check.checkExpr(l, funType, env2)
            (DynamicType, env1)
          case _ =>
            val (resTys, _) = if (occurrence.eqwater(l.clauses)) {
              val envs = occurrence.clausesEnvs(l.clauses, argTys, env1)
              l.clauses
                .lazyZip(envs)
                .map((clause, occEnv) => elabClause(clause, argTys, occEnv, Set.empty))
                .unzip
            } else
              l.clauses.map(elabClause(_, argTys, env1, Set.empty)).unzip
            (subtype.join(resTys), env1)
        }
      case DynCall(dynRemoteFun: DynRemoteFun, args) =>
        if (pipelineContext.gradualTyping) {
          val (_argTys, env1) = elabExprs(args, env)
          (DynamicType, env1)
        } else
          throw NoDynamicRemoteFun(dynRemoteFun.pos, dynRemoteFun)
      case DynCall(f, args) =>
        val (ty, env1) = elabExpr(f, env)
        val expArity = args.size
        if (!util.isFunType(ty, expArity)) {
          throw ExpectedFunType(f.pos, f, expArity, ty)
        }
        val funTys = narrow.asFunType(ty, args.size).get
        if (funTys.isEmpty) {
          val (_, env2) = elabExprs(args, env1)
          (NoneType, env2)
        } else {
          val (argTys, env2) = elabExprs(args, env1)
          val resTys = funTys.map(elabApply.elabApply(_, args, argTys, env2))
          (subtype.join(resTys), env2)
        }
      case DynRemoteFun(mod, name) =>
        throw new IllegalStateException(s"unexpected $expr")
      case DynRemoteFunArity(mod, name, arityExpr) =>
        if (pipelineContext.gradualTyping) {
          val env1 = check.checkExpr(mod, AtomType, env)
          val env2 = check.checkExpr(name, AtomType, env1)
          val env3 = check.checkExpr(arityExpr, NumberType, env2)
          val funType =
            arityExpr match {
              case IntLit(Some(arity)) =>
                FunType(Nil, List.fill(arity)(DynamicType), DynamicType)
              case _ =>
                AnyFunType
            }
          (funType, env3)
        } else
          throw NoDynamicRemoteFun(expr.pos, expr)
      case RemoteCall(RemoteId("eqwalizer", "reveal_type", 1), List(expr)) =>
        val (t, _) = elabExpr(expr, env)
        throw RevealTypeHint(t)(expr.pos)(pipelineContext)
      case RemoteCall(fqn, args) =>
        if (elabApplyCustom.isCustom(fqn)) {
          elabApplyCustom.elabCustom(fqn, args, env, expr.pos)
        } else if (elabApplyOverloaded.isOverloadedFun(fqn)) {
          elabApplyOverloaded.elabOverloaded(expr, fqn, args, env)
        } else
          util.getFunType(fqn) match {
            case Some(ft) =>
              val (argTys, env1) = elabExprs(args, env)
              var resTy = elabApply.elabApply(check.freshen(ft), args, argTys, env1)
              if (CustomReturn.isCustomReturn(fqn))
                resTy = CustomReturn.customizeResultType(fqn, args, resTy)
              (resTy, env1)
            case None =>
              throw UnboundVar(expr.pos, fqn.toString)
          }
      case LocalFun(id) =>
        util.getFunType(module, id) match {
          case Some(ft) =>
            (check.freshen(ft), env)
          case None =>
            throw UnboundVar(expr.pos, id.toString)
        }
      case RemoteFun(fqn) =>
        util.getFunType(fqn) match {
          case Some(ft) =>
            (check.freshen(ft), env)
          case None =>
            throw UnboundVar(expr.pos, fqn.toString)
        }
      case lambda @ Lambda(clauses) =>
        val arity = clauses.head.pats.length
        val funType = if (pipelineContext.gradualTyping) {
          FunType(Nil, List.fill(arity)(DynamicType), DynamicType)
        } else {
          FunType(Nil, List.fill(arity)(NoneType), AnyType)
        }
        val env1 = lambda.name match {
          case Some(name) if pipelineCtx.gradualTyping =>
            env.updated(name, funType)
          case _ =>
            env
        }
        if (arity == 0) {
          val clauseTys = lambda.clauses.map(elabClause(_, Nil, env1, Set.empty)).map(_._1)
          val resTy = subtype.join(clauseTys)
          (FunType(Nil, Nil, resTy), env)
        } else {
          check.checkExpr(lambda, funType, env1)
          (funType, env)
        }
      case Block(block) =>
        elabBody(block, env)
      case c: Case if Predicates.isCaseIf(c) =>
        val ifExpr = Predicates.asIf(c)
        elabExpr(ifExpr, env)
      case c @ Case(sel, clauses) =>
        val (selTy, env1) = elabExpr(sel, env)
        val effVars = Vars.clausesVars(clauses)
        if (occurrence.eqwater(clauses)) {
          val clauseEnvs = occurrence.caseEnvs(c, selTy, env1)
          val (ts, envs) = clauses
            .lazyZip(clauseEnvs)
            .map((clause, occEnv) => elabClause(clause, List(selTy), occEnv, effVars))
            .unzip
          (subtype.join(ts), subtype.joinEnvs(envs))
        } else {
          val (ts, envs) = clauses.map(elabClause(_, List(selTy), env1, effVars)).unzip
          (subtype.join(ts), subtype.joinEnvs(envs))
        }
      case i @ If(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        if (occurrence.eqwater(clauses)) {
          val clauseEnvs = occurrence.ifEnvs(i, env)
          val (ts, envs) = clauses
            .lazyZip(clauseEnvs)
            .map((clause, occEnv) => elabClause(clause, List.empty, occEnv, effVars))
            .unzip
          (subtype.join(ts), subtype.joinEnvs(envs))
        } else {
          val (ts, envs) = clauses.map(elabClause(_, List.empty, env, effVars)).unzip
          (subtype.join(ts), subtype.joinEnvs(envs))
        }
      case Match(mPat @ Pats.PatVar(_), l: Lambda) if pipelineContext.gradualTyping =>
        val arity = l.clauses.head.pats.size
        val gradualFunType = FunType(List.empty, List.fill(arity)(DynamicType), DynamicType)
        val env1 =
          l.name match {
            case Some(name) =>
              env.updated(name, gradualFunType)
            case _ =>
              env
          }
        check.checkExpr(l, gradualFunType, env1)
        val (patTy, patEnv) = elabPat.elabPat(mPat, gradualFunType, env)
        (patTy, patEnv)
      case Match(mPat, mExp) =>
        val (ty, env1) = elabExpr(mExp, env)
        val (patTy, patEnv) = elabPat.elabPat(mPat, ty, env1)
        (patTy, patEnv)
      case UnOp(op, arg) =>
        op match {
          case "not" =>
            val env1 = check.checkExpr(arg, booleanType, env)
            (booleanType, env1)
          case "bnot" | "-" | "+" =>
            val env1 = check.checkExpr(arg, NumberType, env)
            (NumberType, env1)
          case _ =>
            throw UnhandledOp(expr.pos, op)
        }
      case BinOp("orelse", testArg, RemoteCall(RemoteId("erlang", "throw" | "error" | "exit", _), _))
          if Filters.asTest(testArg).isDefined =>
        val test = Filters.asTest(testArg).get
        val env1 = elabGuard.elabGuards(List(Guard(List(test))), env)(checkRedundancy = true)
        (AtomLitType("true"), env1)
      case BinOp("andalso", testArg, RemoteCall(RemoteId("erlang", "throw" | "error" | "exit", _), _))
          if Filters.asTest(testArg).isDefined =>
        val test = Filters.asTest(testArg).get
        val env1 = occurrence.testEnv(test, env, result = false)
        (AtomLitType("false"), env1)
      case BinOp(op, arg1, arg2) =>
        op match {
          case "+" | "-" | "*" | "/" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
            val env1 = check.checkExpr(arg1, NumberType, env)
            val env2 = check.checkExpr(arg2, NumberType, env1)
            (NumberType, env2)
          case "or" | "and" | "xor" =>
            val env1 = check.checkExpr(arg1, booleanType, env)
            val env2 = check.checkExpr(arg2, booleanType, env1)
            (booleanType, env2)
          case "orelse" =>
            val env1 = check.checkExpr(arg1, booleanType, env)
            Filters.asTest(arg1) match {
              case Some(test) =>
                val ifClause1 =
                  Clause(List.empty, List(Guard(List(test))), Body(List(AtomLit("true")(arg1.pos))))(arg1.pos)
                val ifClause2 = Clause(List.empty, List.empty, Body(List(arg2)))(arg2.pos)
                val ifExpr = If(List(ifClause1, ifClause2))(expr.pos)
                elabExpr(ifExpr, env)
              case None =>
                val (t2, env2) = elabExpr(arg2, env1)
                (subtype.join(trueType, t2), env2)
            }
          case "andalso" =>
            val (t1, env1) = elabExpr(arg1, env)
            if (!subtype.subType(t1, booleanType))
              throw ExpectedSubtype(arg1.pos, arg1, expected = booleanType, got = t1)
            val env1Refined = Filters.asTest(arg1) match {
              case None =>
                env1
              case Some(test) =>
                val env11 = elabGuard.elabGuards(List(Guard(List(test))), env1)
                env11
            }
            val t1False = subtype.subType(t1, falseType) && !subtype.subType(t1, trueType)
            if (t1False)
              (falseType, env1)
            else {
              val (t2, _) = elabExpr(arg2, env1Refined)
              val t1True = subtype.subType(t1, trueType) && !subtype.subType(t1, falseType)
              if (t1True)
                (t2, env1)
              else
                (subtype.join(falseType, t2), env1)
            }
          case ">" | "<" | "/=" | ">=" | "=<" | "=/=" | "=:=" | "==" =>
            val (t1, env1) = elabExpr(arg1, env)
            val (t2, env2) = elabExpr(arg2, env1)
            (booleanType, env2)
          case "!" =>
            val sendCall = RemoteCall(RemoteId("erlang", "send", 2), List(arg1, arg2))(expr.pos)
            elabExpr(sendCall, env)
          case "++" | "--" =>
            val (arg1Ty, env1) = elabExpr(arg1, env)
            if (!subtype.subType(arg1Ty, ListType(AnyType)))
              throw ExpectedSubtype(arg1.pos, arg1, expected = ListType(AnyType), got = arg1Ty)
            val (arg2Ty, env2) = elabExpr(arg2, env1)
            if (!subtype.subType(arg2Ty, ListType(AnyType)))
              throw ExpectedSubtype(arg2.pos, arg2, expected = ListType(AnyType), got = arg2Ty)
            val resTy =
              if (op == "--")
                arg1Ty
              else {
                val Some(ListType(elem1Ty)) = narrow.asListType(arg1Ty)
                val Some(ListType(elem2Ty)) = narrow.asListType(arg2Ty)
                ListType(subtype.join(elem1Ty, elem2Ty))
              }
            (resTy, env2)
          case _ =>
            throw UnhandledOp(expr.pos, op)
        }
      case Binary(elems) =>
        var envAcc = env
        for { elem <- elems } {
          val (_, env1) = elabBinaryElem(elem, envAcc)
          envAcc = env1
        }
        (BinaryType, envAcc)
      case Catch(cExpr) =>
        val (strictType, _) = elabExpr(cExpr, env)
        val resultType = if (pipelineContext.gradualTyping) UnionType(Set(strictType, DynamicType)) else AnyType
        (resultType, env)
      case TryCatchExpr(tryBody, catchClauses, afterBody) =>
        val (tryT, _) = elabBody(tryBody, env)
        val stackType = if (pipelineContext.gradualTyping) clsExnStackTypeDynamic else clsExnStackType
        val (catchTs, _) = catchClauses.map(elabClause(_, List(stackType), env, Set.empty)).unzip
        val env1 = afterBody match {
          case Some(block) => elabBody(block, env)._2
          case None        => env
        }
        (subtype.join(tryT :: catchTs), env1)
      case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
        val (tryT, tryEnv) = elabBody(tryBody, env)
        val stackType = if (pipelineContext.gradualTyping) clsExnStackTypeDynamic else clsExnStackType
        if (occurrence.eqwater(tryClauses)) {
          val tryEnvs = occurrence.clausesEnvs(tryClauses, List(tryT), tryEnv)
          val (tryTs, _) =
            tryClauses
              .lazyZip(tryEnvs)
              .map((clause, occEnv) => elabClause(clause, List(tryT), occEnv, Set.empty))
              .unzip
          val (catchTs, _) = catchClauses.map(elabClause(_, List(stackType), env, Set.empty)).unzip
          val env1 = afterBody match {
            case Some(block) => elabBody(block, env)._2
            case None        => env
          }
          (subtype.join(tryTs ::: catchTs), env1)
        } else {
          val (tryTs, _) = tryClauses.map(elabClause(_, List(tryT), tryEnv, Set.empty)).unzip
          val (catchTs, _) = catchClauses.map(elabClause(_, List(stackType), env, Set.empty)).unzip
          val env1 = afterBody match {
            case Some(block) => elabBody(block, env)._2
            case None        => env
          }
          (subtype.join(tryTs ::: catchTs), env1)
        }
      case Receive(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        val argType = if (pipelineContext.gradualTyping) DynamicType else AnyType
        val (ts, envs) = clauses.map(elabClause(_, List(argType), env, effVars)).unzip
        (subtype.join(ts), subtype.joinEnvs(envs))
      case ReceiveWithTimeout(List(), timeout, timeoutBlock) =>
        val env1 = check.checkExpr(timeout, builtinTypes("timeout"), env)
        elabBody(timeoutBlock, env1)
      case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
        val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
        val argType = if (pipelineContext.gradualTyping) DynamicType else AnyType
        val (ts, envs) = clauses.map(elabClause(_, List(argType), env, effVars)).unzip
        val env1 = check.checkExpr(timeout, builtinTypes("timeout"), env)
        val (timeoutT, timeoutEnv) = elabBody(timeoutBlock, env1)
        (subtype.join(timeoutT :: ts), subtype.joinEnvs(timeoutEnv :: envs))
      case LComprehension(template, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        val (tType, _) = elabExpr(template, qEnv)
        (ListType(tType), env)
      case BComprehension(template, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        check.checkExpr(template, BinaryType, qEnv)
        (BinaryType, env)
      case MComprehension(kTemplate, vTemplate, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        val (kType, _) = elabExpr(kTemplate, qEnv)
        val (vType, _) = elabExpr(vTemplate, qEnv)
        (DictMap(kType, vType), env)
      case rCreate: RecordCreate =>
        elabRecordCreate(rCreate, env)
      case rUpdate: RecordUpdate =>
        elabRecordUpdate(rUpdate, env)
      case RecordSelect(recExpr, recName, fieldName) =>
        val recDecl = util.getRecord(module, recName).getOrElse(throw UnboundRecord(expr.pos, recName))
        val (elabTy, elabEnv) = elabExpr(recExpr, env)
        if (subtype.subType(elabTy, RecordType(recName)(module)))
          (narrow.getRecordField(recDecl, elabTy, fieldName), elabEnv)
        else
          throw ExpectedSubtype(recExpr.pos, recExpr, expected = RecordType(recName)(module), got = elabTy)
      case RecordIndex(_, _) =>
        (NumberType, env)
      case MapCreate(kvs) =>
        val isShape = kvs.forall(_._1.isInstanceOf[AtomLit])
        var envAcc = env
        if (isShape) {
          val props = kvs.collect { case (AtomLit(key), value) =>
            val (valT, env1) = elabExpr(value, envAcc)
            envAcc = env1
            ReqProp(key, valT)
          }
          (ShapeMap(props), envAcc)
        } else {
          val (keyTs, valTs) = kvs.map { case (key, value) =>
            val (keyT, env1) = elabExpr(key, envAcc)
            val (valT, env2) = elabExpr(value, env1)
            envAcc = env2
            (keyT, valT)
          }.unzip
          val domain = keyTs.reduce(subtype.join)
          val codomain = valTs.reduce(subtype.join)
          (DictMap(domain, codomain), envAcc)
        }
      case MapUpdate(map, kvs) =>
        val (mapT, env1) = elabExpr(map, env)
        val anyMap = DictMap(AnyType, AnyType)
        if (!subtype.subType(mapT, anyMap)) {
          throw ExpectedSubtype(map.pos, map, expected = anyMap, got = mapT)
        }
        var envAcc = env1
        var resT = mapT
        for ((key, value) <- kvs) {
          val (keyT, env2) = elabExpr(key, envAcc)
          val (valT, env3) = elabExpr(value, env2)
          envAcc = env3
          resT = narrow.adjustMapType(resT, keyT, valT)
        }
        (resT, envAcc)
      case MaybeMatch(mPat, mExp) =>
        val (mType, env1) = elabExpr(mExp, env)
        elabPat.elabPat(mPat, mType, env1)
      case Maybe(body) =>
        elabMaybeBody(body, env)
      case MaybeElse(body, elseClauses) =>
        val (bodyType, _) = elabBody(body, env)
        val argType = if (pipelineContext.gradualTyping) DynamicType else AnyType
        val (ts, _) = elseClauses.map(elabClause(_, List(argType), env, Set.empty)).unzip
        (subtype.join(bodyType :: ts), env)
    }

  def elabBinaryElem(elem: BinaryElem, env: Env): (Type, Env) = {
    val env1 = elem.size match {
      case Some(s) => check.checkExpr(s, NumberType, env)
      case None    => env
    }
    val isStringLiteral = elem.expr.isInstanceOf[StringLit]
    val expType = BinarySpecifiers.expType(elem.specifier, isStringLiteral)
    val env2 = check.checkExpr(elem.expr, expType, env1)
    (expType, env2)
  }

  def elabRecordCreate(rCreate: RecordCreate, env: Env): (Type, Env) = {
    val RecordCreate(recName, fields) = rCreate
    val recType = RecordType(recName)(module)
    val namedFields = fields.collect { case n: RecordFieldNamed => n }
    val genFieldOpt = fields.collectFirst { case g: RecordFieldGen => g }
    val recDecl = util.getRecord(module, recName).getOrElse(throw UnboundRecord(rCreate.pos, recName))
    var refinedFields: Map[String, Type] = Map.empty

    var envAcc = env

    genFieldOpt match {
      case Some(genField) =>
        val genNames = (recDecl.fields.keySet -- namedFields.map(_.name)).toList.sorted
        for (genName <- genNames) {
          val fieldDecl = recDecl.fields(genName)
          if (fieldDecl.refinable) {
            val (fTy, fEnv) = elabExpr(genField.value, envAcc)
            if (!subtype.subType(fTy, fieldDecl.tp))
              throw ExpectedSubtype(genField.value.pos, genField.value, expected = fieldDecl.tp, got = fTy)
            refinedFields += (fieldDecl.name -> fTy)
            envAcc = fEnv
          } else {
            envAcc = check.checkExpr(genField.value, fieldDecl.tp, envAcc)
          }
        }
      case None =>
        val undefinedFields = (recDecl.fields.keySet -- namedFields.map(_.name)).toList.sorted
        for (uField <- undefinedFields) {
          val fieldDecl = recDecl.fields(uField)
          val refinable = fieldDecl.refinable
          fieldDecl.defaultValue match {
            case None =>
              if (!subtype.subType(undefined, fieldDecl.tp))
                throw UndefinedField(rCreate.pos, recName, uField)
              if (refinable)
                refinedFields += (uField -> undefined)
            case Some(defVal) =>
              val (valTy, envVal) = elabExpr(defVal, env)
              if (!subtype.subType(valTy, fieldDecl.tp))
                throw ExpectedSubtype(defVal.pos, defVal, expected = fieldDecl.tp, got = valTy)
              if (refinable)
                refinedFields += (uField -> valTy)
              envAcc = envVal
          }
        }
    }

    for (namedField <- namedFields) {
      val fieldDecl = recDecl.fields(namedField.name)
      if (fieldDecl.refinable) {
        val (fTy, fEnv) = elabExpr(namedField.value, envAcc)
        if (!subtype.subType(fTy, fieldDecl.tp))
          throw ExpectedSubtype(namedField.value.pos, namedField.value, expected = fieldDecl.tp, got = fTy)
        refinedFields += (fieldDecl.name -> fTy)
        envAcc = fEnv
      } else {
        envAcc = check.checkExpr(namedField.value, fieldDecl.tp, envAcc)
      }
    }

    if (refinedFields.isEmpty) (recType, envAcc)
    else (RefinedRecordType(recType, refinedFields), envAcc)
  }

  def elabRecordUpdate(rUpdate: RecordUpdate, env: Env): (Type, Env) = {
    val RecordUpdate(recExpr, recName, fields) = rUpdate
    val recType = RecordType(recName)(module)
    val recDecl = util.getRecord(module, recName).getOrElse(throw UnboundRecord(rUpdate.pos, recName))
    var refinedFields: Map[String, Type] = Map.empty
    var envAcc = Env.empty
    if (recDecl.refinable) {
      val (refTy, refEnv) = elabExpr(recExpr, env)
      val allRefinedFields = recDecl.fields.collect { case (name, f) if f.refinable => name }.toSet
      val keepFields = allRefinedFields -- fields.map(_.name)
      if (!subtype.subType(refTy, recType))
        throw ExpectedSubtype(recExpr.pos, recExpr, expected = recType, got = refTy)
      keepFields.foreach { fieldName =>
        val fieldTy = narrow.getRecordField(recDecl, refTy, fieldName)
        refinedFields += (fieldName -> fieldTy)
      }
      envAcc = refEnv
    } else {
      envAcc = check.checkExpr(recExpr, recType, env)
    }
    for (field <- fields) {
      val fieldDecl = recDecl.fields(field.name)
      if (fieldDecl.refinable) {
        val (fTy, fEnv) = elabExpr(field.value, envAcc)
        if (!subtype.subType(fTy, fieldDecl.tp))
          throw ExpectedSubtype(field.value.pos, field.value, expected = fieldDecl.tp, got = fTy)
        refinedFields += (fieldDecl.name -> fTy)
        envAcc = fEnv
      } else {
        envAcc = check.checkExpr(field.value, fieldDecl.tp, envAcc)
      }
    }
    if (refinedFields.isEmpty) (recType, envAcc)
    else (RefinedRecordType(recType, refinedFields), envAcc)
  }

  def elabQualifiers(qualifiers: List[Qualifier], env: Env): Env = {
    var envAcc = env
    qualifiers.foreach {
      case LGenerate(gPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc)
        if (!subtype.subType(gT, ListType(AnyType)))
          throw ExpectedSubtype(gExpr.pos, gExpr, expected = ListType(AnyType), got = gT)
        val Some(ListType(gElemT)) = narrow.asListType(gT)
        val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
        envAcc = pEnv
      case BGenerate(gPat, gExpr) =>
        envAcc = check.checkExpr(gExpr, BinaryType, envAcc)
        val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
        envAcc = pEnv
      case MGenerate(gkPat, gvPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc)
        val anyMap = DictMap(AnyType, AnyType)
        if (!subtype.subType(gT, anyMap)) {
          throw ExpectedSubtype(gExpr.pos, gExpr, expected = anyMap, got = gT)
        }
        val mapT = narrow.asMapType(gT)
        val kT = narrow.getKeyType(mapT)
        val vT = narrow.getValType(mapT)
        val (_, kPatEnv) = elabPat.elabPat(gkPat, kT, gEnv)
        val (_, vPatEnv) = elabPat.elabPat(gvPat, vT, kPatEnv)
        envAcc = vPatEnv
      case Filter(fExpr) =>
        Filters.asTest(fExpr).foreach { test =>
          envAcc = elabGuard.elabGuards(List(Guard(List(test))), envAcc)
        }
        envAcc = elabExpr(fExpr, envAcc)._2
    }
    envAcc
  }
}
