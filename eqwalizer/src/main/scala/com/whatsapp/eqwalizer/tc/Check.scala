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
import com.whatsapp.eqwalizer.ast.{Filters, Pats, RemoteId, TypeVars, Vars}
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
  private lazy val narrow = pipelineContext.narrow
  private lazy val occurrence = pipelineContext.occurrence
  private lazy val customReturn = pipelineContext.customReturn
  private lazy val typeInfo = pipelineContext.typeInfo
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  lazy val freshen = new TypeVars.VarFreshener().freshen _
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def checkFun(f: FunDecl, spec: FunSpec): Unit = {
    val ft = freshen(spec.ty)
    val FunType(_, argTys, resTy) = ft
    val clauseEnvs = occurrence.clausesEnvs(f.clauses, ft.argTys, Map.empty)
    val singleClause = f.clauses.length == 1
    f.clauses
      .lazyZip(1 to f.clauses.length)
      .lazyZip(clauseEnvs)
      .foreach((clause, index, occEnv) =>
        checkClause(
          clause,
          argTys,
          resTy,
          occEnv,
          Set.empty,
          checkCoverage = singleClause || (index != f.clauses.length),
        )
      )
  }

  def checkOverloadedFun(f: FunDecl, overloadedSpec: OverloadedFunSpec): Unit = {
    overloadedSpec.tys.foreach { funTy =>
      val ft = freshen(funTy)
      val FunType(_, argTys, resTy) = ft
      val clauseEnvs = occurrence.clausesEnvs(f.clauses, ft.argTys, Env.empty)
      f.clauses
        .lazyZip(clauseEnvs)
        .map((clause, occEnv) => checkClauseOverloadedClause(clause, argTys, resTy, occEnv))
    }
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

  private def checkMaybeBody(body: Body, resTy: Type, env: Env): Env = {
    var envAcc = env
    var lastTy: Type = NoneType
    val exprs = body.exprs
    for (expr <- exprs) {
      expr match {
        case MaybeMatch(Pats.PatAtom("true"), mExp) if Filters.asTest(mExp).isDefined =>
          val test = Filters.asTest(mExp).get
          val env1 = elabGuard.elabGuards(List(Guard(List(test))), env)
          lastTy = trueType
          envAcc = env1
        case MaybeMatch(mPat, mExp) =>
          val (mType, env1) = elab.elabExprAndCheck(mExp, envAcc, resTy)
          val (patTy, env2) = elabPat.elabPat(mPat, mType, env1)
          lastTy = patTy
          envAcc = env2
        case _ =>
          val (exprTy, env1) = elab.elabExpr(expr, envAcc)
          lastTy = exprTy
          envAcc = env1
      }
    }
    if (!subtype.subType(lastTy, resTy))
      diagnosticsInfo.add(ExpectedSubtype(exprs.last.pos, exprs.last, expected = resTy, got = lastTy))
    env
  }

  def checkClause(
      clause: Clause,
      argTys: List[Type],
      resTy: Type,
      env0: Env,
      exportedVars: Set[String],
      checkCoverage: Boolean = false,
  ): Env = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(env0, patVars)
    // see D29637051 for why we elabGuard twice
    val env2 = typeInfo.withoutTypeCollection {
      elabGuard.elabGuards(clause.guards, env1)
    }
    val (_, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    val env4 = elabGuard.elabGuards(clause.guards, env3)
    if (pipelineContext.clauseCoverage && checkCoverage && env4.exists { case (_, ty) => subtype.isNoneType(ty) }) {
      diagnosticsInfo.add(ClauseNotCovered(clause.pos))
    }
    val env5 = checkBody(clause.body, resTy, env4)
    util.exitScope(env0, env5, exportedVars)
  }

  private def checkClauseOverloadedClause(
      clause: Clause,
      argTys: List[Type],
      resTy: Type,
      env: Env,
  ): Unit = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(env, patVars)
    val env2 = typeInfo.withoutTypeCollection {
      elabGuard.elabGuards(clause.guards, env1)
    }
    val (patTys, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    val reachable = !patTys.exists(subtype.isNoneType)
    if (reachable) {
      // elabGuard twice for the same reasons as above, see D43679406
      val env4 = elabGuard.elabGuards(clause.guards, env3)
      checkBody(clause.body, resTy, env4)
    }
  }

  def checkExpr(expr: Expr, resTy: Type, env: Env): Env = {
    if (subtype.subType(AnyType, resTy)) elab.elabExpr(expr, env)._2
    else
      expr match {
        case Var(v) =>
          val vt = env.getOrElse(v, { diagnosticsInfo.add(UnboundVar(expr.pos, v)); DynamicType })
          typeInfo.add(expr.pos, vt)
          if (!subtype.subType(vt, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = vt))
          env
        case AtomLit(a) =>
          val litType = AtomLitType(a)
          if (!subtype.subType(litType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType))
          env
        case FloatLit() =>
          val litType = NumberType
          if (!subtype.subType(litType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType))
          env
        case IntLit(_) =>
          val litType = NumberType
          if (!subtype.subType(litType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType))
          env
        case Tuple(elems) =>
          var envAcc = env
          val elemTypes = elems.map { elem =>
            val (elemType, env1) = elab.elabExpr(elem, envAcc)
            envAcc = env1
            elemType
          }
          val tupleType = TupleType(elemTypes)
          if (!subtype.subType(tupleType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = tupleType))
          envAcc
        case StringLit(empty) =>
          val litType = if (empty) NilType else stringType
          if (!subtype.subType(litType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType))
          env
        case NilLit() =>
          val litType = NilType
          if (!subtype.subType(litType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = litType))
          env
        case LocalCall(id, args) =>
          val funId = util.globalFunId(module, id)
          if (elabApplyCustom.isCustom(funId)) {
            val (callTy, env1) = elabApplyCustom.elabCustom(funId, args, env, expr.pos)
            if (!subtype.subType(callTy, resTy))
              diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy))
            env1
          } else if (elabApplyOverloaded.isOverloadedFun(funId)) {
            val (callTy, env1) = elabApplyOverloaded.elabOverloaded(expr, funId, args, env)
            if (!subtype.subType(callTy, resTy))
              diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy))
            env1
          } else {
            val ft = util.getFunType(module, id)
            checkApply(funId, expr, freshen(ft), args, resTy, env)
          }
        case DynRemoteFun(mod, name) =>
          throw new IllegalStateException(s"unexpected $expr")
        case dFun: DynRemoteFunArity =>
          val (funTy, env1) = elab.elabExpr(dFun, env)
          if (!subtype.subType(funTy, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = funTy))
          env1
        case RemoteCall(RemoteId("eqwalizer", "reveal_type", 1), List(expr)) =>
          val (t, env1) = elab.elabExpr(expr, env)
          diagnosticsInfo.add(RevealTypeHint(t)(expr.pos)(pipelineContext))
          env1
        case RemoteCall(fqn, args) =>
          if (elabApplyCustom.isCustom(fqn)) {
            val (callTy, env1) = elabApplyCustom.elabCustom(fqn, args, env, expr.pos)
            if (!subtype.subType(callTy, resTy))
              diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy))
            env1
          } else if (elabApplyOverloaded.isOverloadedFun(fqn)) {
            val (callTy, env1) = elabApplyOverloaded.elabOverloaded(expr, fqn, args, env)
            if (!subtype.subType(callTy, resTy))
              diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = callTy))
            env1
          } else {
            val ft = util.getFunType(fqn)
            checkApply(fqn, expr, freshen(ft), args, resTy, env)
          }
        case DynCall(l: Lambda, args) =>
          val arity = lambdaArity(l)
          val (argTys, env1) = elab.elabExprs(args, env)
          if (arity != args.size) {
            diagnosticsInfo.add(LambdaArityMismatch(l.pos, l, lambdaArity = arity, argsArity = args.size))
            return env1
          }
          l.name match {
            case Some(name) =>
              val funType = FunType(Nil, List.fill(argTys.size)(DynamicType), resTy)
              val env2 = env.updated(name, funType)
              checkExpr(l, funType, env2)
            case _ =>
              val envs = occurrence.clausesEnvs(l.clauses, argTys, env1)
              l.clauses
                .lazyZip(envs)
                .map((clause, occEnv) => checkClause(clause, argTys, resTy, occEnv, Set.empty))
          }
          env1
        case DynCall(dynRemoteFun: DynRemoteFun, args) =>
          val (_argTys, env1) = elab.elabExprs(args, env)
          // dynamic is subtype of everything, - no need to double-check
          env1
        case DynCall(f, args) =>
          val (ty, env1) = elab.elabExpr(f, env)
          val expArity = args.size
          val funTy =
            if (!util.isFunType(ty, expArity)) {
              diagnosticsInfo.add(ExpectedFunType(f.pos, f, expArity, ty))
              DynamicType
            } else {
              ty
            }
          val funTys = narrow.extractFunTypes(funTy, args.size)
          val (argTys, env2) = elab.elabExprs(args, env1)
          if (funTys.nonEmpty) {
            funTys.foreach { ft =>
              val ftResTy = elabApply.elabApply(ft, args, argTys, env1)
              if (!subtype.subType(ftResTy, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = ftResTy))
            }
          }
          env2
        case LocalFun(id) =>
          val fqn = util.globalFunId(module, id)
          if (pipelineCtx.ignoredOverloadedSpec && util.getOverloadedSpec(fqn).isDefined) {
            diagnosticsInfo.add(IgnoredOverloadedSpec(expr.pos))
          }
          val ft = util.getFunType(fqn)
          val ft1 = freshen(ft)
          if (!subtype.subType(ft1, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = ft1))
          env
        case RemoteFun(fqn) =>
          val ft = util.getFunType(fqn)
          if (pipelineCtx.ignoredOverloadedSpec && util.getOverloadedSpec(fqn).isDefined) {
            diagnosticsInfo.add(IgnoredOverloadedSpec(expr.pos))
          }
          val ft1 = freshen(ft)
          if (!subtype.subType(ft1, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = ft1))
          env
        case lambda: Lambda =>
          checkLambda(lambda, resTy, env)
        case Block(block) =>
          checkBody(block, resTy, env)
        case c: Case if Predicates.isCaseIf(c) =>
          // Elaborate test expression to store its type info
          val (_, _) = elab.elabExpr(c.expr, env)
          val ifExpr = Predicates.asIf(c)
          checkExpr(ifExpr, resTy, env)
        case Case(call @ RemoteCall(id, args), clauses)
            if Predicates.booleanClauses(clauses) && elabApplyCustom.isCustomPredicate(id) =>
          val (_, posEnv, negEnv) = elabApplyCustom.elabCustomPredicate(id, args, env, call.pos)
          val (posClause, negClause) = Predicates.posNegClauses(clauses)
          val effVars = Vars.clausesVars(clauses)
          val posEnv1 =
            checkClause(posClause, List(booleanType), resTy, posEnv, effVars)
          val negEnv1 =
            checkClause(negClause, List(booleanType), resTy, negEnv, effVars)
          subtype.joinEnvs(List(posEnv1, negEnv1))
        case c @ Case(sel, clauses) =>
          val (selType, env1) = elab.elabExpr(sel, env)
          val effVars = Vars.clausesVars(clauses)
          if (occurrence.eqwater(clauses)) {
            val clauseEnvs = pipelineCtx.occurrence.caseEnvs(c, selType, env1)
            val envs2 = clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => checkClause(clause, List(selType), resTy, occEnv, effVars))
            subtype.joinEnvs(envs2)
          } else {
            val envs2 = clauses.map(checkClause(_, List(selType), resTy, env1, effVars))
            subtype.joinEnvs(envs2)
          }
        case i @ If(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          if (occurrence.eqwater(clauses)) {
            val clauseEnvs = pipelineCtx.occurrence.ifEnvs(i, env)
            val envs1 = clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => checkClause(clause, List.empty, resTy, occEnv, effVars))
            subtype.joinEnvs(envs1)
          } else {
            val envs1 = clauses.map(checkClause(_, List.empty, resTy, env, effVars))
            subtype.joinEnvs(envs1)
          }
        case Match(mPat, mExp) =>
          val (mType, env1) = elab.elabExpr(mExp, env)
          val (t2, env2) = elabPat.elabPat(mPat, mType, env1)
          if (!subtype.subType(t2, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = t2))
          env2
        case _: UnOp | _: BinOp =>
          elab.elabExprAndCheck(expr, env, resTy)._2
        case Binary(elems) =>
          if (!subtype.subType(BinaryType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = BinaryType))
          var envAcc = env
          for { elem <- elems } {
            val (_, env1) = elab.elabBinaryElem(elem, envAcc)
            envAcc = env1
          }
          envAcc
        case Catch(_) =>
          elab.elabExprAndCheck(expr, env, resTy)._2
        case TryCatchExpr(tryBody, catchClauses, afterBody) =>
          checkBody(tryBody, resTy, env)
          val stackType = clsExnStackTypeDynamic
          catchClauses.map(checkClause(_, List(stackType), resTy, env, Set.empty))
          afterBody match {
            case Some(block) => elab.elabBody(block, env)._2
            case None        => env
          }
        case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
          val (tryBodyT, tryEnv) = elab.elabBody(tryBody, env)
          val stackType = clsExnStackTypeDynamic
          val tryEnvs = occurrence.clausesEnvs(tryClauses, List(tryBodyT), tryEnv)
          tryClauses
            .lazyZip(tryEnvs)
            .map((clause, occEnv) => checkClause(clause, List(tryBodyT), resTy, occEnv, Set.empty))
          catchClauses.map(checkClause(_, List(stackType), resTy, env, Set.empty))
          afterBody match {
            case Some(block) => elab.elabBody(block, env)._2
            case None        => env
          }
        case Receive(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          val argType = DynamicType
          val envs1 = clauses.map(checkClause(_, List(argType), resTy, env, effVars))
          subtype.joinEnvs(envs1)
        case ReceiveWithTimeout(List(), timeout, timeoutBlock) =>
          val env1 = checkExpr(timeout, builtinTypes("timeout"), env)
          checkBody(timeoutBlock, resTy, env1)
        case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
          val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
          val argType = DynamicType
          val envs1 = clauses.map(checkClause(_, List(argType), resTy, env, effVars))
          val tEnv1 = checkExpr(timeout, builtinTypes("timeout"), env)
          val tEnv2 = checkBody(timeoutBlock, resTy, tEnv1)
          val tEnv3 = util.exitScope(env, tEnv2, effVars)
          subtype.joinEnvs(tEnv3 :: envs1)
        case LComprehension(template, qualifiers) =>
          val qEnv = elab.elabQualifiers(qualifiers, env)
          val (tType, _) = elab.elabExpr(template, qEnv)
          val elabType = ListType(tType)
          if (!subtype.subType(elabType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = elabType))
          env
        case BComprehension(template, qualifiers) =>
          if (!subtype.subType(BinaryType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = BinaryType))
          val qEnv = elab.elabQualifiers(qualifiers, env)
          checkExpr(template, BinaryType, qEnv)
          env
        case m: MComprehension =>
          val (elabType, env1) = elab.elabExpr(m, env)
          if (!subtype.subType(elabType, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = elabType))
          env
        case rCreate: RecordCreate =>
          val recDeclOpt = util.getRecord(module, rCreate.recName)
          recDeclOpt match {
            case Some(recDecl) if recDecl.refinable =>
              val (recType, envCreate) = elab.elabRecordCreate(rCreate, env)
              if (!subtype.subType(recType, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType))
              envCreate
            case Some(recDecl) =>
              val recType = RecordType(rCreate.recName)(module)
              if (!subtype.subType(recType, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType))
              elab.elabRecordCreate(rCreate, env)._2
            case None =>
              diagnosticsInfo.add(UnboundRecord(expr.pos, rCreate.recName))
              env
          }
        case rUpdate: RecordUpdate =>
          val recDeclOpt = util.getRecord(module, rUpdate.recName)
          recDeclOpt match {
            case Some(recDecl) if recDecl.refinable =>
              val (recType, envUpdate) = elab.elabRecordUpdate(rUpdate, env)
              if (!subtype.subType(recType, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType))
              envUpdate
            case Some(_) =>
              val recType = RecordType(rUpdate.recName)(module)
              if (!subtype.subType(recType, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = recType))
              elab.elabRecordUpdate(rUpdate, env)._2
            case None =>
              diagnosticsInfo.add(UnboundRecord(expr.pos, rUpdate.recName))
              env
          }
        case RecordSelect(recExpr, recName, fieldName) =>
          val recDeclOpt = util.getRecord(module, recName)
          val (elabTy, elabEnv) = elab.elabExpr(recExpr, env)
          recDeclOpt match {
            case Some(recDecl) =>
              val (elabTy, elabEnv) = elab.elabExprAndCheck(recExpr, env, RecordType(recName)(module))
              val fieldTy = narrow.getRecordField(recDecl, elabTy, fieldName)
              if (!subtype.subType(fieldTy, resTy))
                diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = fieldTy))
              elabEnv
            case None =>
              diagnosticsInfo.add(UnboundRecord(expr.pos, recName))
              env
          }
        case RecordIndex(_, _) =>
          val indT = NumberType
          if (!subtype.subType(indT, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = indT))
          env
        case MaybeMatch(mPat, mExp) =>
          val (mType, env1) = elab.elabExpr(mExp, env)
          val (t2, env2) = elabPat.elabPat(mPat, mType, env1)
          if (!subtype.subType(t2, resTy))
            diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = t2))
          env2
        case Maybe(body) =>
          checkMaybeBody(body, resTy, env)
        case MaybeElse(body, elseClauses) =>
          checkBody(body, resTy, env)
          val argType = DynamicType
          elseClauses.foreach(checkClause(_, List(argType), resTy, env, Set.empty))
          env
        case _: MapCreate | _: MapUpdate | _: Cons =>
          // delegating this stuff to elaborate for now
          elab.elabExprAndCheck(expr, env, resTy)._2
      }
  }

  private def lambdaArity(lambda: Lambda): Int = lambda.clauses.head.pats.size

  private def checkLambdaFunType(lambda: Lambda, funTy: FunType, env: Env): Env = {
    val FunType(_, fParamTys, fResTy) = funTy
    val arity = lambdaArity(lambda)
    if (arity != fParamTys.size) {
      diagnosticsInfo.add(LambdaArityMismatch(lambda.pos, lambda, lambdaArity = arity, argsArity = fParamTys.size))
      return env
    }
    val env1 = lambda.name match {
      case Some(name) =>
        env.updated(name, funTy)
      case _ =>
        env
    }
    val envs = occurrence.clausesEnvs(lambda.clauses, fParamTys, env1)
    lambda.clauses
      .lazyZip(envs)
      .map((clause, occEnv) => checkClause(clause, fParamTys, fResTy, occEnv, Set.empty))
    env
  }

  def checkLambda(lambda: Lambda, resTy: Type, env: Env): Env = {
    resTy match {
      case t: FunType =>
        checkLambdaFunType(lambda, t, env)
      case _ =>
        val arity = lambdaArity(lambda)
        narrow.extractFunTypes(resTy, arity).toList match {
          case List(funTy) => checkLambdaFunType(lambda, funTy, env)
          case _ =>
            val (ty, _) = elab.elabExpr(lambda, env)
            if (!subtype.subType(ty, resTy))
              diagnosticsInfo.add(ExpectedSubtype(lambda.pos, lambda, expected = resTy, got = ty))
        }
    }
    env
  }

  private def checkApply(funId: RemoteId, expr: Expr, ft: FunType, args: List[Expr], resTy: Type, env: Env): Env = {
    val (argTys, env1) = typeInfo.withoutLambdaTypeCollection {
      elab.elabExprs(args, env)
    }
    var ftResTy = elabApply.elabApply(ft, args, argTys, env1)
    if (customReturn.isCustomReturn(funId))
      ftResTy = customReturn.customizeResultType(funId, args, argTys, ftResTy)
    if (!subtype.subType(ftResTy, resTy))
      diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = resTy, got = ftResTy))
    env1
  }
}
