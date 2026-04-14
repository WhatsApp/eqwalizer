/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.*
import com.whatsapp.eqwalizer.ast.Pats.PatVar
import com.whatsapp.eqwalizer.ast.{Subst, Variance}
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{AmbiguousLambda, ExpectedSubtype}
import com.whatsapp.eqwalizer.tc.generics.Constraints
import com.whatsapp.eqwalizer.tc.generics.Constraints.ConstraintSeq

import scala.collection.mutable.ListBuffer

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

  private sealed trait AppliedArg extends Constraints.ConstraintLoc
  private case class LambdaArg(arg: Lambda, argTy: Type, paramTy: FunType) extends AppliedArg
  private case class Arg(arg: Expr, argTy: Type, paramTy: Type) extends AppliedArg

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
  private def lambdaArg(lambda: Lambda, argTy: FunType, paramTy: Type): AppliedArg = {
    val arity = lambda.clauses.headOption.map(_.pats.size).getOrElse(0)
    val funParamTys = narrow.onlyFunTypes(paramTy, arity)
    if (funParamTys.size == 1) {
      LambdaArg(lambda, argTy, funParamTys.head)
    } else if (funParamTys.size > 1) {
      diagnosticsInfo.add(AmbiguousLambda(lambda.pos, lambda, subtype.join(funParamTys)))
      LambdaArg(lambda, argTy, FunType(0, List.fill(arity)(DynamicType), DynamicType))
    } else {
      paramTy match {
        // Keep it as a LambdaArg to produce an arity mismatch error message, which provides clearer signal
        case ft: FunType => LambdaArg(lambda, argTy, ft)
        case t if subtype.gradualSubType(DynamicType, t) =>
          LambdaArg(lambda, argTy, FunType(0, List.fill(arity)(DynamicType), DynamicType))
        case _ => Arg(lambda, argTy, paramTy)
      }
    }
  }

  def elabApply(ft: FunType, args: List[Expr], argTys: List[Type], env: Env): Type = {
    assert(ft.argTys.size == argTys.size)
    assert(args.size == argTys.size)
    if (ft.forall == 0)
      elabApplyMono(ft, args, argTys, env)
    else
      elabApplyPoly(ft, args, argTys, env)
  }

  private def elabApplyMono(ft: FunType, args: List[Expr], argTys: List[Type], env: Env): Type = {
    val appliedArgs = args
      .zip(argTys)
      .zip(ft.argTys)
      .map {
        case ((lambda: Lambda, argTy: FunType), paramTy) if argTy.argTys.nonEmpty =>
          lambdaArg(lambda, argTy, paramTy)
        case ((expr, argTy), paramTy) => Arg(expr, argTy, paramTy)
      }

    val lambdaArgs = appliedArgs.collect { case la: LambdaArg => la }
    val nonLambdaArgs = appliedArgs.collect { case pa: Arg => pa }

    nonLambdaArgs.foreach(checkArg(_, None))
    lambdaArgs.foreach(checkLambdaArg(_, None, env))

    ft.resTy
  }

  private def elabApplyPoly(ft0: FunType, args: List[Expr], argTys: List[Type], env: Env): Type = {

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
        case ((expr, argTy), paramTy) => Arg(expr, argTy, paramTy)
      }

    val lambdaArgs = appliedArgs.collect { case la: LambdaArg => la }
    val nonLambdaArgs = appliedArgs.collect { case pa: Arg => pa }
    val variances = Variance.toVariances(ft, vars)

    // First we elaborate non-lambda arguments (datums)
    val (cs1, subst1) = elabTerms(toSolve, nonLambdaArgs, variances)
    // Then we elaborate the lambdas using the partial solutions
    val (cs3, subst3) = typeInfo.withoutTypeCollection {
      val (cs2, subst2) = elabLambdas(cs1, subst1, lambdaArgs, env, variances, toSolve)
      val subst2Merged = subst2.map {
        case (v, UnionType(tys)) => (v, narrow.joinAndMergeMaps(tys))
        case (v, ty)             => (v, ty)
      }
      elabLambdas(cs2, subst2Merged, lambdaArgs, env, variances, toSolve)
    }

    // We check that all arguments are well-typed under the final substitution.
    // These checks are necessary because:
    // - A type can be both an input and an output of a lambda.
    // - (optimization) We exit constraint generation early if there are no type variables: in such cases we still need to check
    // that the args match the parameters.
    // - We assume that any consistent substitution of type variables is sound.
    //   For example, we use an approximation for `meet` in Constraints.scala
    nonLambdaArgs.foreach(checkArg(_, Some(subst3)))
    lambdaArgs.foreach(checkLambdaArg(_, Some(subst3), env))

    Subst.subst(subst3, ft.resTy)
  }

  private def elabTerms(
      toSolve: Set[Var],
      args: List[Arg],
      variances: Map[Var, Variance],
  ): (ConstraintSeq, Map[Var, Type]) = {
    val delayed: ListBuffer[Arg] = ListBuffer.empty
    val cs1 = args.foldLeft(Vector.empty: ConstraintSeq) { case (cs, arg) =>
      try
        constraints.constraintGen(
          toSolve,
          cs = cs,
          variances = variances,
          lower = arg.argTy,
          upper = arg.paramTy,
          constraintLoc = arg,
          tolerateUnion = true,
        )
      catch {
        case Constraints.UnionFailure() =>
          delayed.addOne(arg)
          cs
      }
    }

    val m1 = constraints.meetAllConstraints(cs1, variances, Map.empty)
    val subst1 = constraints.constraintsToSubst(m1, variances)

    // Process "delayed arguments" that have union upper bounds
    val cs2 = delayed.toList.foldLeft(cs1) { case (cs, arg) =>
      constraints.constraintGen(
        toSolve,
        cs = cs,
        variances = variances,
        lower = arg.argTy,
        upper = Subst.subst(subst1, arg.paramTy),
        constraintLoc = arg,
        tolerateUnion = false,
      )
    }

    val m2 = constraints.meetAllConstraints(cs2, variances, m1)
    val subst2 = constraints.constraintsToSubst(m2, variances, toSolve)
    (cs2, subst2)
  }

  private def elabLambdas(
      cs: ConstraintSeq,
      subst: Map[Var, Type],
      lambdaArgs: List[LambdaArg],
      env: Env,
      variances: Map[Var, Variance],
      toSolve: Set[Var],
  ): (ConstraintSeq, Map[Var, Type]) = {
    var cs1 = cs
    for (lambdaArg <- lambdaArgs) {
      val funType = lambdaToFunTy(lambdaArg, subst, env)
      cs1 = constraints.constraintGen(
        toSolve,
        lower = funType.resTy,
        upper = lambdaArg.paramTy.resTy,
        lambdaArg.copy(argTy = funType),
        cs1,
        variances,
        false,
      )
    }
    (cs1, constraints.constraintsSeqToSubst(cs1, variances, toSolve))
  }

  private def checkArg(arg: Arg, varToType: Option[Map[Var, Type]]): Unit = {
    val Arg(expr, argTy, rawParamTy) = arg
    val paramTy = varToType.fold(rawParamTy)(Subst.subst(_, rawParamTy))
    if (!subtype.subType(argTy, paramTy))
      diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = paramTy, got = argTy))
  }

  private def checkLambdaArg(lambdaArg: LambdaArg, varToType: Option[Map[Var, Type]], env: Env): Unit = {
    val LambdaArg(lambda, _, FunType(_, rawArgTys, rawExpResTy)) = lambdaArg
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
    check.checkLambda(lambda, expFunTy, env1)
  }

  private def lambdaToFunTy(lambdaArg: LambdaArg, subst: Map[Var, Type], env: Env): FunType = {
    val LambdaArg(lambda, _, ft: FunType) = lambdaArg

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
