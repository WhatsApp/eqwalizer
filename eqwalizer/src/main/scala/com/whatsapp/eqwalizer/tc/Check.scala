/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Forms.{FunDecl, FunSpec, OverloadedFunSpec}
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{Filters, RemoteId, TypeVars, Vars}
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

final class Check(pipelineContext: PipelineContext) {
  private lazy val module = pipelineContext.module
  private lazy val elab = pipelineContext.elab
  private lazy val elabApply = pipelineContext.elabApply
  private lazy val elabApplyCustom = pipelineContext.elabApplyCustom
  private lazy val elabApplyOverloaded = pipelineContext.elabApplyOverloaded
  private lazy val elabGuard = pipelineContext.elabGuard
  private lazy val elabPat = pipelineContext.elabPat
  private lazy val subtype = pipelineContext.subtype
  private lazy val util = pipelineContext.util
  private lazy val approx = pipelineContext.approx
  private lazy val occurrence = pipelineContext.occurrence
  lazy val freshen = new TypeVars.VarFreshener().freshen _
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def checkFun(f: FunDecl, spec: FunSpec): Unit = {
    val ft = freshen(spec.ty)
    val FunType(_, argTys, resTy) = ft
    if (occurrence.eqwater(f.clauses)) {
      val clauseEnvs = occurrence.clausesEnvs(f.clauses, ft.argTys, Map.empty)
      f.clauses
        .lazyZip(clauseEnvs)
        .map((clause, occEnv) => checkClause(clause, argTys, resTy, occEnv, Set.empty))
    } else {
      f.clauses.map(checkClause(_, argTys, resTy, Env.empty, Set.empty))
    }
  }

  def checkOverloadedFun(f: FunDecl, overloadedSpec: OverloadedFunSpec): Unit =
    overloadedSpec.tys.foreach { funTy =>
      val ft = freshen(funTy)
      val FunType(_, argTys, resTy) = ft
      f.clauses.map(checkClauseOverloadedClause(_, argTys, resTy))
    }

  private def checkBody(body: Body, resTy: Type, env: Env): Env = {
    var envAcc = env
    val exprs = body.exprs
    for (expr <- exprs.init) {
      val (_, env1) = elab.elabExpr(expr, envAcc)
      envAcc = env1
    }
    checkExpr(exprs.last, resTy, envAcc)
  }

  private def checkClause(
      clause: Clause,
      argTys: List[Type],
      resTy: Type,
      env0: Env,
      exportedVars: Set[String],
  ): Env = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(env0, patVars)
    // see D29637051 for why we elabGuard twice
    val env2 = elabGuard.elabGuards(clause.guards, env1)
    val (_, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    val env4 = elabGuard.elabGuards(clause.guards, env3)
    val env5 = checkBody(clause.body, resTy, env4)
    util.exitScope(env0, env5, exportedVars)
  }

  private def checkClauseOverloadedClause(
      clause: Clause,
      argTys: List[Type],
      resTy: Type,
  ): Unit = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(Map.empty, patVars)
    val env2 = elabGuard.elabGuards(clause.guards, env1)
    val (patTys, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    val reachable = !patTys.exists(subtype.isNoneType)
    if (reachable)
      checkBody(clause.body, resTy, env3)
  }

  def checkExpr(expr: Expr, resTy: Type, env: Env): Env =
    if (subtype.subType(AnyType, resTy)) elab.elabExpr(expr, env)._2
    else
      expr match {
        case Var(v) =>
          val vt = env.getOrElse(v, throw UnboundVar(expr.pos, v))
          if (subtype.subType(vt, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = vt)
        case AtomLit(a) =>
          val litType = AtomLitType(a)
          if (subtype.subType(litType, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType)
        case FloatLit() =>
          val litType = NumberType
          if (subtype.subType(litType, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType)
        case IntLit(_) =>
          val litType = NumberType
          if (subtype.subType(litType, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType)
        case Tuple(elems) =>
          var envAcc = env
          val elemTypes = elems.map { elem =>
            val (elemType, env1) = elab.elabExpr(elem, envAcc)
            envAcc = env1
            elemType
          }
          val tupleType = TupleType(elemTypes)
          if (subtype.subType(tupleType, resTy)) envAcc
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = tupleType)
        case StringLit(empty) =>
          val litType = if (empty) NilType else stringType
          if (subtype.subType(litType, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType)
        case NilLit() =>
          val litType = NilType
          if (subtype.subType(litType, resTy)) env
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType)
        case Cons(head, tail) =>
          val (headType, env1) = elab.elabExpr(head, env)
          val typeList1 = ListType(headType)
          if (!subtype.subType(typeList1, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = typeList1)
          checkExpr(tail, resTy, env1)
        case LocalCall(id, args) =>
          val funId = util.globalFunId(module, id)
          if (elabApplyCustom.isCustom(funId)) {
            val (callTy, env1) = elabApplyCustom.elabCustom(funId, args, env, expr.pos)
            if (!subtype.subType(callTy, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy)
            env1
          } else if (elabApplyOverloaded.isOverloadedFun(funId)) {
            val (callTy, env1) = elabApplyOverloaded.elabOverloaded(expr, funId, args, env)
            if (!subtype.subType(callTy, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy)
            env1
          } else
            util.getFunType(module, id) match {
              case Some(ft) =>
                checkApply(expr, freshen(ft), args, resTy, env)
              case None =>
                throw UnboundVar(expr.pos, id.toString)
            }
        // $COVERAGE-OFF$
        case DynRemoteFun(mod, name) =>
          throw new IllegalStateException(s"unexpected $expr")
        // $COVERAGE-ON$
        case dFun: DynRemoteFunArity =>
          val (funTy, env1) = elab.elabExpr(dFun, env)
          if (!subtype.subType(funTy, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = funTy)
          env1
        case RemoteCall(RemoteId("eqwalizer", "reveal_type", 1), List(expr)) =>
          val (t, _) = elab.elabExpr(expr, env)
          throw RevealTypeHint(t)(expr.pos)(pipelineContext)
        case RemoteCall(fqn, args) =>
          if (elabApplyCustom.isCustom(fqn)) {
            val (callTy, env1) = elabApplyCustom.elabCustom(fqn, args, env, expr.pos)
            if (!subtype.subType(callTy, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy)
            env1
          } else if (elabApplyOverloaded.isOverloadedFun(fqn)) {
            val (callTy, env1) = elabApplyOverloaded.elabOverloaded(expr, fqn, args, env)
            if (!subtype.subType(callTy, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy)
            env1
          } else
            util.getFunType(fqn) match {
              case Some(ft) =>
                checkApply(expr, freshen(ft), args, resTy, env)
              case None =>
                throw UnboundVar(expr.pos, fqn.toString)
            }
        case DynCall(l: Lambda, args) =>
          val arity = lambdaArity(l)
          if (arity != args.size) throw LambdaArityMismatch(l.pos, l, lambdaArity = arity, argsArity = args.size)
          val (argTys, env1) = elab.elabExprs(args, env)
          l.name match {
            case Some(name) if pipelineCtx.gradualTyping =>
              val funType = FunType(Nil, List.fill(argTys.size)(DynamicType), resTy)
              val env2 = env.updated(name, funType)
              checkExpr(l, funType, env2)
            case _ =>
              if (occurrence.eqwater(l.clauses)) {
                val envs = occurrence.clausesEnvs(l.clauses, argTys, env1)
                l.clauses
                  .lazyZip(envs)
                  .map((clause, occEnv) => checkClause(clause, argTys, resTy, occEnv, Set.empty))
              } else {
                l.clauses.foreach(checkClause(_, argTys, resTy, env1, Set.empty))
              }
          }
          env1
        case DynCall(dynRemoteFun: DynRemoteFun, args) =>
          if (pipelineContext.gradualTyping) {
            val (_argTys, env1) = elab.elabExprs(args, env)
            // dynamic is subtype of everything, - no need to double-check
            env1
          } else
            throw NoDynamicRemoteFun(dynRemoteFun.pos, dynRemoteFun)
        case DynCall(f, args) =>
          val (ty, env1) = elab.elabExpr(f, env)
          val expArity = args.size
          if (!util.isFunType(ty, expArity)) {
            throw ExpectedFunType(f.pos, f, expArity, ty)
          }
          val funTys = approx.asFunType(ty, args.size).get
          if (funTys.isEmpty) {
            val (_, env2) = elab.elabExprs(args, env1)
            env2
          } else {
            val envs = funTys.map(checkApply(expr, _, args, resTy, env1))
            approx.joinEnvs(envs)
          }
        case LocalFun(id) =>
          util.getFunType(module, id) match {
            case Some(ft) =>
              val ft1 = freshen(ft)
              if (subtype.subType(ft1, resTy)) env
              else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = ft1)
            case None =>
              throw UnboundVar(expr.pos, id.toString)
          }
        case RemoteFun(fqn) =>
          util.getFunType(fqn) match {
            case Some(ft) =>
              val ft1 = freshen(ft)
              if (subtype.subType(ft1, resTy)) env
              else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = ft1)
            case None =>
              throw UnboundVar(expr.pos, fqn.toString)
          }
        case lambda: Lambda =>
          checkLambda(lambda, resTy, env)
        case Block(block) =>
          checkBody(block, resTy, env)
        case c: Case if Predicates.isCaseIf(c) =>
          val ifExpr = Predicates.asIf(c)
          checkExpr(ifExpr, resTy, env)
        case c @ Case(sel, clauses) =>
          val (selType, env1) = elab.elabExpr(sel, env)
          val effVars = Vars.clausesVars(clauses)
          if (occurrence.eqwater(clauses)) {
            val clauseEnvs = pipelineCtx.occurrence.caseEnvs(c, selType, env1)
            val envs2 = clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => checkClause(clause, List(selType), resTy, occEnv, effVars))
            approx.joinEnvs(envs2)
          } else {
            val envs2 = clauses.map(checkClause(_, List(selType), resTy, env1, effVars))
            approx.joinEnvs(envs2)
          }
        case i @ If(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          if (occurrence.eqwater(clauses)) {
            val clauseEnvs = pipelineCtx.occurrence.ifEnvs(i, env)
            val envs1 = clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => checkClause(clause, List.empty, resTy, occEnv, effVars))
            approx.joinEnvs(envs1)
          } else {
            val envs1 = clauses.map(checkClause(_, List.empty, resTy, env, effVars))
            approx.joinEnvs(envs1)
          }
        case Match(mPat, mExp) =>
          val (mType, env1) = elab.elabExpr(mExp, env)
          val (t2, env2) = elabPat.elabPat(mPat, mType, env1)
          if (subtype.subType(t2, resTy)) env2
          else throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = t2)
        case _: UnOp | _: BinOp =>
          val (ty, env1) = elab.elabExpr(expr, env)
          if (!subtype.subType(ty, resTy)) {
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = ty)
          }
          env1
        case Binary(elems) =>
          if (!subtype.subType(BinaryType, resTy)) {
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = BinaryType)
          }
          var envAcc = env
          for { elem <- elems } {
            val (_, env1) = elab.elabBinaryElem(elem, envAcc)
            envAcc = env1
          }
          envAcc
        case Catch(_) =>
          val (ty, env1) = elab.elabExpr(expr, env)
          if (!subtype.subType(ty, resTy)) throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = ty)
          env1
        case TryCatchExpr(tryBody, catchClauses, afterBody) =>
          checkBody(tryBody, resTy, env)
          val stackType = if (pipelineContext.gradualTyping) clsExnStackTypeDynamic else clsExnStackType
          catchClauses.map(checkClause(_, List(stackType), resTy, env, Set.empty))
          afterBody match {
            case Some(block) => elab.elabBody(block, env)._2
            case None        => env
          }
        case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
          val (tryBodyT, tryEnv) = elab.elabBody(tryBody, env)
          val stackType = if (pipelineContext.gradualTyping) clsExnStackTypeDynamic else clsExnStackType
          if (occurrence.eqwater(tryClauses)) {
            val tryEnvs = occurrence.clausesEnvs(tryClauses, List(tryBodyT), tryEnv)
            tryClauses
              .lazyZip(tryEnvs)
              .map((clause, occEnv) => checkClause(clause, List(tryBodyT), resTy, occEnv, Set.empty))
            catchClauses.map(checkClause(_, List(stackType), resTy, env, Set.empty))
          } else {
            tryClauses.map(checkClause(_, List(tryBodyT), resTy, tryEnv, Set.empty))
            catchClauses.map(checkClause(_, List(stackType), resTy, env, Set.empty))
          }
          afterBody match {
            case Some(block) => elab.elabBody(block, env)._2
            case None        => env
          }
        case Receive(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          val argType = if (pipelineContext.gradualTyping) DynamicType else AnyType
          val envs1 = clauses.map(checkClause(_, List(argType), resTy, env, effVars))
          approx.joinEnvs(envs1)
        case ReceiveWithTimeout(List(), timeout, timeoutBlock) =>
          val env1 = checkExpr(timeout, builtinTypes("timeout"), env)
          checkBody(timeoutBlock, resTy, env1)
        case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
          val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
          val argType = if (pipelineContext.gradualTyping) DynamicType else AnyType
          val envs1 = clauses.map(checkClause(_, List(argType), resTy, env, effVars))
          val tEnv1 = checkExpr(timeout, builtinTypes("timeout"), env)
          val tEnv2 = checkBody(timeoutBlock, resTy, tEnv1)
          val tEnv3 = util.exitScope(env, tEnv2, effVars)
          approx.joinEnvs(tEnv3 :: envs1)
        case LComprehension(template, qualifiers) =>
          var envAcc = env
          qualifiers.foreach {
            case LGenerate(gPat, gExpr) =>
              val (gT, gEnv) = elab.elabExpr(gExpr, envAcc)
              if (!subtype.subType(gT, ListType(AnyType)))
                throw ExpectedSubtype(gExpr.pos, gExpr, expected = ListType(AnyType), got = gT)
              val Some(ListType(gElemT)) = approx.asListType(gT)
              val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
              envAcc = pEnv
            case BGenerate(gPat, gExpr) =>
              envAcc = checkExpr(gExpr, BinaryType, envAcc)
              val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
              envAcc = pEnv
            case Filter(fExpr) =>
              Filters.asTest(fExpr).foreach { test =>
                val env1 = elabGuard.elabGuards(List(Guard(List(test))), envAcc)
                envAcc = env1
              }
              envAcc = elab.elabExpr(fExpr, envAcc)._2
          }
          val (tType, _) = elab.elabExpr(template, envAcc)
          val elabType = ListType(tType)
          if (!subtype.subType(elabType, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = elabType)
          env
        case BComprehension(template, qualifiers) =>
          if (!subtype.subType(BinaryType, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = BinaryType)
          var envAcc = env
          qualifiers.foreach {
            case LGenerate(gPat, gExpr) =>
              val (gT, gEnv) = elab.elabExpr(gExpr, envAcc)
              if (!subtype.subType(gT, ListType(AnyType)))
                throw ExpectedSubtype(gExpr.pos, gExpr, expected = ListType(AnyType), got = gT)
              val Some(ListType(gElemT)) = approx.asListType(gT)
              val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
              envAcc = pEnv
            case BGenerate(gPat, gExpr) =>
              envAcc = checkExpr(gExpr, BinaryType, envAcc)
              val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
              envAcc = pEnv
            case Filter(fExpr) =>
              Filters.asTest(fExpr).foreach { test =>
                val env1 = elabGuard.elabGuards(List(Guard(List(test))), envAcc)
                envAcc = env1
              }
              envAcc = elab.elabExpr(fExpr, envAcc)._2
          }
          checkExpr(template, BinaryType, envAcc)
          env
        case rCreate: RecordCreate =>
          val recDecl = util
            .getRecord(module, rCreate.recName)
            .getOrElse(throw UnboundRecord(expr.pos, rCreate.recName))
          if (recDecl.refinable) {
            val (recType, envCreate) = elab.elabRecordCreate(rCreate, env)
            if (!subtype.subType(recType, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType)
            envCreate
          } else {
            val recType = RecordType(rCreate.recName)(module)
            if (!subtype.subType(recType, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType)
            elab.elabRecordCreate(rCreate, env)._2
          }
        case rUpdate: RecordUpdate =>
          val recDecl = util
            .getRecord(module, rUpdate.recName)
            .getOrElse(throw UnboundRecord(expr.pos, rUpdate.recName))
          if (recDecl.refinable) {
            val (recType, envUpdate) = elab.elabRecordUpdate(rUpdate, env)
            if (!subtype.subType(recType, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType)
            envUpdate
          } else {
            val recType = RecordType(rUpdate.recName)(module)
            if (!subtype.subType(recType, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType)
            elab.elabRecordUpdate(rUpdate, env)._2
          }
        case RecordSelect(recExpr, recName, fieldName) =>
          val recDecl = util
            .getRecord(module, recName)
            .getOrElse(throw UnboundRecord(expr.pos, recName))
          val field = recDecl.fields(fieldName)
          if (field.refinable) {
            val (elabTy, elabEnv) = elab.elabExpr(recExpr, env)
            approx.getRecordField(recDecl, elabTy, fieldName) match {
              case None =>
                throw ExpectedSubtype(recExpr.pos, recExpr, expected = RecordType(recName)(module), got = elabTy)
              case Some(fieldTy) =>
                if (!subtype.subType(fieldTy, resTy))
                  throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = fieldTy)
                elabEnv
            }
          } else {
            val fieldT = field.tp
            if (!subtype.subType(fieldT, resTy))
              throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = fieldT)
            else
              checkExpr(recExpr, RecordType(recName)(module), env)
          }
        case RecordIndex(_, _) =>
          val indT = NumberType
          if (!subtype.subType(indT, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = indT)
          else
            env
        case _: MapCreate | _: GenMapUpdate | _: ReqMapUpdate =>
          // delegating all map stuff to elaborate for now
          val (mapT, env1) = elab.elabExpr(expr, env)
          if (!subtype.subType(mapT, resTy))
            throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = mapT)
          else
            env1
      }

  private def lambdaArity(lambda: Lambda): Int = lambda.clauses.head.pats.size

  def checkLambda(lambda: Lambda, resTy: Type, env: Env): Env = {
    resTy match {
      case FunType(_, fParamTys, fResTy) =>
        val arity = lambdaArity(lambda)
        if (arity != fParamTys.size)
          throw LambdaArityMismatch(lambda.pos, lambda, lambdaArity = arity, argsArity = fParamTys.size)
        val env1 = lambda.name match {
          case Some(name) =>
            env.updated(name, resTy)
          case _ =>
            env
        }
        if (occurrence.eqwater(lambda.clauses)) {
          val envs = occurrence.clausesEnvs(lambda.clauses, fParamTys, env1)
          lambda.clauses
            .lazyZip(envs)
            .map((clause, occEnv) => checkClause(clause, fParamTys, fResTy, occEnv, Set.empty))
        } else {
          lambda.clauses.foreach(checkClause(_, fParamTys, fResTy, env1, exportedVars = Set.empty))
        }
      case _ =>
        val (ty, _) = elab.elabExpr(lambda, env)
        if (!subtype.subType(ty, resTy))
          throw ExpectedSubtype(lambda.pos, lambda, expected = resTy, got = ty)
    }
    env
  }

  private def checkApply(expr: Expr, ft: FunType, args: List[Expr], resTy: Type, env: Env): Env = {
    val (argTys, env1) = elab.elabExprs(args, env)
    val ftResTy = elabApply.elabApply(ft, args, argTys, env1)
    if (!subtype.subType(ftResTy, resTy)) throw ExpectedSubtype(expr.pos, expr, expected = resTy, got = ftResTy)
    env1
  }
}
