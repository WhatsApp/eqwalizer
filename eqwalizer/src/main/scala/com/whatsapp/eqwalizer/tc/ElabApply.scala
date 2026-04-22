/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.*
import com.whatsapp.eqwalizer.ast.Pats.PatVar
import com.whatsapp.eqwalizer.ast.{Pos, Subst, Variance}
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{AmbiguousLambda, ExpectedSubtype}
import com.whatsapp.eqwalizer.tc.Constraints.CMap

class ElabApply(pipelineContext: PipelineContext) {
  private lazy val check = pipelineContext.check
  private lazy val elab = pipelineContext.elab
  private lazy val subtype = pipelineContext.subtype
  private lazy val constraints = pipelineContext.constraints
  private lazy val occurrence = pipelineContext.occurrence
  private lazy val narrow = pipelineContext.narrow
  private lazy val typeInfo = pipelineContext.typeInfo
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  private lazy val instantiate = pipelineContext.instantiate
  implicit val pipelineCtx: PipelineContext = pipelineContext

  private type Var = Int

  private sealed trait Arg
  private case class LambdaArg(lambda: Lambda, funType: FunType) extends Arg
  private case class TermArg(arg: Expr, argTy: Type, paramTy: Type) extends Arg

  private def etaExpand(fun: LocalFun): Lambda = {
    val pos = fun.pos
    val varNames = (1 to fun.id.arity).map(n => s"Arg $n of '${fun.id}'").toList
    val vars = varNames.map(Var(_)(pos))
    val patVars = varNames.map(PatVar(_)(pos))
    val app = LocalCall(fun.id, vars)(pos)
    val clause = Clause(patVars, Nil, Body(List(app)))(pos)
    Lambda(List(clause))(pos, name = None)
  }
  private def etaExpand(fun: RemoteFun): Lambda = {
    val pos = fun.pos
    val varNames = (1 to fun.id.arity).map(n => s"Arg $n of '${fun.id}'").toList
    val patVars = varNames.map(PatVar(_)(pos))
    val vars = varNames.map(Var(_)(pos))
    val app = RemoteCall(fun.id, vars)(pos)
    val clause = Clause(patVars, Nil, Body(List(app)))(pos)
    Lambda(List(clause))(pos, name = None)
  }
  private def lambdaArg(lambda: Lambda, argTy: FunType, paramTy: Type): Arg = {
    val arity = lambda.clauses.headOption.map(_.pats.size).getOrElse(0)
    val funParamTys = narrow.onlyFunTypes(paramTy, arity)
    if (funParamTys.size == 1) {
      LambdaArg(lambda, funParamTys.head)
    } else if (funParamTys.size > 1) {
      diagnosticsInfo.add(AmbiguousLambda(lambda.pos, lambda, subtype.join(funParamTys)))
      LambdaArg(lambda, FunType(0, List.fill(arity)(DynamicType), DynamicType))
    } else {
      paramTy match {
        // Keep it as a LambdaArg to produce an arity mismatch error message, which provides clearer signal
        case ft: FunType => LambdaArg(lambda, ft)
        case t if subtype.gradualSubType(DynamicType, t) =>
          LambdaArg(lambda, FunType(0, List.fill(arity)(DynamicType), DynamicType))
        case _ => TermArg(lambda, argTy, paramTy)
      }
    }
  }

  def elabApply(ft: FunType, args: List[Expr], argTys: List[Type], env: Env, pos: Pos): Type = {
    assert(ft.argTys.size == argTys.size)
    assert(args.size == argTys.size)
    if (ft.forall == 0)
      elabApplyMono(ft, args, argTys, env)
    else
      elabApplyPoly(ft, args, argTys, env, pos)
  }

  private def elabApplyMono(ft: FunType, args: List[Expr], argTys: List[Type], env: Env): Type = {
    val appliedArgs = args
      .zip(argTys)
      .zip(ft.argTys)
      .map {
        case ((lambda: Lambda, argTy: FunType), paramTy) if argTy.argTys.nonEmpty =>
          lambdaArg(lambda, argTy, paramTy)
        case ((expr, argTy), paramTy) => TermArg(expr, argTy, paramTy)
      }

    val termArgs = appliedArgs.collect { case pa: TermArg => pa }
    val lambdaArgs = appliedArgs.collect { case la: LambdaArg => la }

    termArgs.foreach(checkTermArg(_, None))
    lambdaArgs.foreach(checkLambdaArg(_, None, env))

    ft.resTy
  }

  private def elabApplyPoly(ft0: FunType, args: List[Expr], argTys: List[Type], env: Env, pos: Pos): Type = {

    val (vars, ft) = instantiate.instantiate(ft0)
    val toSolve = vars.toSet

    val appliedArgs = args
      .zip(argTys)
      .zip(ft.argTys)
      .map {
        case ((lambda: Lambda, argTy: FunType), paramTy) if argTy.argTys.nonEmpty =>
          lambdaArg(lambda, argTy, paramTy)
        case ((fun: LocalFun, argTy: FunType), paramTy) if argTy.forall > 0 =>
          lambdaArg(etaExpand(fun), argTy, paramTy)
        case ((fun: RemoteFun, argTy: FunType), paramTy) if argTy.forall > 0 =>
          lambdaArg(etaExpand(fun), argTy, paramTy)
        case ((expr, argTy), paramTy) => TermArg(expr, argTy, paramTy)
      }

    val termArgs = appliedArgs.collect { case pa: TermArg => pa }
    val lambdaArgs = appliedArgs.collect { case la: LambdaArg => la }
    val variances = Variance.toVariances(ft, vars)

    val (solutions1: List[(CMap, Map[Var, Type])], termsSuccess) = elabTerms(toSolve, termArgs, variances) match {
      case Some(tuples1) => (tuples1, true)
      case None          => (List((Map.empty: CMap, toSolve.map(_ -> DynamicType).toMap)), false)
    }

    // Then we elaborate the lambdas using the partial solutions
    val elabLambdasRes = typeInfo.withoutTypeCollection {
      for {
        (cs1, subst1) <- solutions1
        solutions2: List[(CMap, Map[Var, Type])] <- elabLambdas(cs1, subst1, lambdaArgs, env, variances, toSolve)
        (cs2, subst2) <- solutions2
        subst2Merged = subst2.map {
          case (v, UnionType(tys)) => (v, narrow.joinAndMergeMaps(tys))
          case (v, ty)             => (v, ty)
        }
        solutions3: List[(CMap, Map[Var, Type])] <- elabLambdas(cs2, subst2Merged, lambdaArgs, env, variances, toSolve)
        (cs3, subst3) <- solutions3
      } yield (cs3, subst3)
    }

    // in the end we take the first solution if it exists
    val (subst3, lambdasSuccess) = elabLambdasRes match {
      case (_, subst3) :: _ => (subst3, true)
      case Nil              => (toSolve.map(_ -> DynamicType).toMap, false)
    }
    // We check that all arguments are well-typed under the final substitution.
    val termsTyped = termArgs.map(checkTermArg(_, Some(subst3))).forall(identity)
    val lambdasTyped = lambdaArgs.map(checkLambdaArg(_, Some(subst3), env)).forall(identity)

    if (!termsSuccess && termsTyped) termArgs.foreach(checkTermArg(_, None))
    else if (!lambdasSuccess && lambdasTyped) lambdaArgs.foreach(checkLambdaArg(_, None, env))

    Subst.subst(subst3, ft.resTy)
  }

  private def elabTerms(
      toSolve: Set[Var],
      args: List[TermArg],
      variances: Map[Var, Variance],
  ): Option[List[(CMap, Map[Var, Type])]] = {
    val cs1 = args.foldLeft(Option(List(Map.empty: CMap))) { case (csOpt, arg) =>
      for {
        cs <- csOpt
        delta <- constraints.constraintGen(
          toSolve,
          lower = arg.argTy,
          upper = arg.paramTy,
        )
        meet <- constraints.meetConstraints(cs, delta)
      } yield meet
    }
    cs1 match {
      case None      => None
      case Some(cs1) => Some(cs1.map(cs => (cs, constraints.constraintsToSubst(cs, variances, toSolve))))
    }
  }

  private def elabLambdas(
      cs: CMap,
      subst: Map[Var, Type],
      lambdaArgs: List[LambdaArg],
      env: Env,
      variances: Map[Var, Variance],
      toSolve: Set[Var],
  ): Option[List[(CMap, Map[Var, Type])]] = {
    var cs1 = Option(List(cs))
    for (lambdaArg <- lambdaArgs) {
      val funType = lambdaToFunTy(lambdaArg, subst, env)
      cs1 = for {
        cs <- cs1;
        delta <-
          constraints.constraintGen(
            toSolve,
            lower = funType.resTy,
            upper = lambdaArg.funType.resTy,
          )
        meet <- constraints.meetConstraints(cs, delta)
      } yield meet
    }
    cs1 match {
      case None      => None
      case Some(cs1) => Some(cs1.map(cs => (cs, constraints.constraintsToSubst(cs, variances, toSolve))))
    }
  }

  private def checkTermArg(arg: TermArg, varToType: Option[Map[Var, Type]]): Boolean = {
    val TermArg(expr, argTy, rawParamTy) = arg
    val paramTy = varToType.fold(rawParamTy)(Subst.subst(_, rawParamTy))
    if (!subtype.subType(argTy, paramTy)) {
      diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = paramTy, got = argTy))
      false
    } else true
  }

  private def checkLambdaArg(lambdaArg: LambdaArg, varToType: Option[Map[Var, Type]], env: Env): Boolean = {
    val LambdaArg(lambda, FunType(_, rawArgTys, rawExpResTy)) = lambdaArg
    val argTys = varToType.fold(rawArgTys)(s => rawArgTys.map(Subst.subst(s, _)))
    val expResTy = varToType.fold(rawExpResTy)(Subst.subst(_, rawExpResTy))
    val expFunTy = FunType(0, argTys, expResTy)
    val env1 =
      lambda.name match {
        case Some(name) =>
          val funType = FunType(0, List.fill(argTys.size)(DynamicType), DynamicType)
          env.updated(name, funType)
        case _ =>
          env
      }
    val (_, typed) = check.checkLambda(lambda, expFunTy, env1)
    typed
  }

  private def lambdaToFunTy(lambdaArg: LambdaArg, subst: Map[Var, Type], env: Env): FunType = {
    val LambdaArg(lambda, ft: FunType) = lambdaArg

    val argTys = ft.argTys.map(Subst.subst(subst, _))
    val env1 =
      lambda.name match {
        case Some(name) =>
          val funType = FunType(0, List.fill(argTys.size)(DynamicType), DynamicType)
          env.updated(name, funType)
        case _ =>
          env
      }

    val envs = occurrence.clausesEnvs(lambda.clauses, argTys, env1)
    val clauseTys =
      lambda.clauses
        .lazyZip(envs)
        .map((clause, occEnv) => elab.elabClause(clause, argTys, occEnv, Set.empty))
        .map(_._1)

    val resTy = subtype.join(clauseTys)
    ft.copy(argTys = argTys, resTy = resTy)
  }
}
