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
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{AmbiguousLambda, ExpectedSubtype, NoSolution}
import com.whatsapp.eqwalizer.tc.generics.Constraints
import com.whatsapp.eqwalizer.tc.generics.Constraints.CMap

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

    val (cs1, subst1, termsSuccess) = elabTerms(toSolve, termArgs, variances) match {
      case Some(cs1, subst1) =>
        (cs1, subst1, true)
      case None =>
        (Map.empty: CMap, toSolve.map(_ -> DynamicType).toMap, false)
    }

    // Then we elaborate the lambdas using the partial solutions
    val elabLambdasRes = typeInfo.withoutTypeCollection {
      elabLambdas(cs1, subst1, lambdaArgs, env, variances, toSolve) match {
        case Some((cs2, subst2)) =>
          val subst2Merged = subst2.map {
            case (v, UnionType(tys)) => (v, narrow.joinAndMergeMaps(tys))
            case (v, ty)             => (v, ty)
          }
          elabLambdas(cs2, subst2Merged, lambdaArgs, env, variances, toSolve)
        case None =>
          None
      }
    }

    val (cs3, subst3, lambdasSuccess) = elabLambdasRes match {
      case Some((cs3, subst3)) =>
        (cs3, subst3, true)
      case None =>
        (Map.empty: CMap, toSolve.map(_ -> DynamicType).toMap, false)
    }
    // We check that all arguments are well-typed under the final substitution.
    // These checks are necessary because:
    // - A type can be both an input and an output of a lambda.
    // - (optimization) We exit constraint generation early if there are no type variables: in such cases we still need to check
    // that the args match the parameters.
    // - We assume that any consistent substitution of type variables is sound.
    //   For example, we use an approximation for `meet` in Constraints.scala
    val termsTyped = termArgs.map(checkTermArg(_, Some(subst3))).forall(identity)
    val lambdasTyped = lambdaArgs.map(checkLambdaArg(_, Some(subst3), env)).forall(identity)

    if ((!termsSuccess && termsTyped) || (!lambdasSuccess && lambdasTyped)) {
      diagnosticsInfo.add(NoSolution(pos))
    }

    Subst.subst(subst3, ft.resTy)
  }

  private def elabTerms(
      toSolve: Set[Var],
      args: List[TermArg],
      variances: Map[Var, Variance],
  ): Option[(CMap, Map[Var, Type])] = {
    val delayed: ListBuffer[TermArg] = ListBuffer.empty
    val cs1 = args.foldLeft(Option(Map.empty: CMap)) { case (csOpt, arg) =>
      try {
        for {
          cs <- csOpt
          delta <- constraints.constraintGen(
            toSolve,
            lower = arg.argTy,
            upper = arg.paramTy,
            tolerateUnion = true,
          )
          meet <- constraints.meetConstraints(cs, delta)
        } yield meet
      } catch {
        case Constraints.UnionFailure() =>
          delayed.addOne(arg)
          csOpt
      }
    }

    cs1 match {
      case None =>
        None
      case Some(cs1) =>
        val subst1 = constraints.constraintsToSubst(cs1, variances)
        // Process "delayed arguments" that have union upper bounds
        val cs2 = delayed.toList.foldLeft(Option(cs1)) { case (csOpt, arg) =>
          for {
            cs <- csOpt
            delta <-
              constraints.constraintGen(
                toSolve,
                lower = arg.argTy,
                upper = Subst.subst(subst1, arg.paramTy),
                tolerateUnion = false,
              )
            meet <- constraints.meetConstraints(cs, delta)
          } yield meet
        }
        cs2 match {
          case None =>
            None
          case Some(cs2) =>
            val subst2 = constraints.constraintsToSubst(cs2, variances, toSolve)
            Some(cs2, subst2)
        }
    }
  }

  private def elabLambdas(
      cs: CMap,
      subst: Map[Var, Type],
      lambdaArgs: List[LambdaArg],
      env: Env,
      variances: Map[Var, Variance],
      toSolve: Set[Var],
  ): Option[(CMap, Map[Var, Type])] = {
    var cs1 = Option(cs)
    for (lambdaArg <- lambdaArgs) {
      val funType = lambdaToFunTy(lambdaArg, subst, env)
      cs1 = for {
        cs <- cs1;
        delta <-
          constraints.constraintGen(
            toSolve,
            lower = funType.resTy,
            upper = lambdaArg.funType.resTy,
            false,
          )
        meet <- constraints.meetConstraints(cs, delta)
      } yield meet
    }
    cs1 match {
      case None =>
        None
      case Some(cs1) =>
        val sub = constraints.constraintsToSubst(cs1, variances, toSolve)
        Some(cs1, sub)
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
