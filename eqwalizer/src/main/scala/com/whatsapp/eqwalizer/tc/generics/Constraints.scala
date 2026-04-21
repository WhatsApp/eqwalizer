/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.{Subst, TypeVars, Variance}
import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.PipelineContext
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{AmbiguousUnion, ExpectedSubtype}

import scala.annotation.tailrec

object Constraints {
  private type Var = Int

  trait ConstraintLoc {
    val arg: Expr
    val argTy: Type
    val paramTy: Type
  }

  case class Constraint(lower: Type, upper: Type)

  type ConstraintSeq = Vector[(Var, ConstraintLoc, Constraint)]

  private case class State(
      toSolve: Set[Var],
      varsToElim: Set[Var],
      cs: ConstraintSeq,
      variances: Map[Var, Variance],
      constraintLoc: ConstraintLoc,
  )

  private case class SubtypeFailure() extends Exception
  case class UnionFailure() extends Exception
}

class Constraints(pipelineContext: PipelineContext) {
  import Constraints._
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  private lazy val instantiate = pipelineContext.instantiate
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def constraintGen(
      toSolve: Set[Var],
      lower: Type,
      upper: Type,
      constraintLoc: ConstraintLoc,
      cs: ConstraintSeq,
      variances: Map[Var, Variance],
      tolerateUnion: Boolean,
  ): ConstraintSeq = {
    val state = State(
      toSolve = toSolve,
      varsToElim = Set.empty,
      cs,
      variances,
      constraintLoc,
    )
    try {
      constrain(state, lower, upper, Set.empty, tolerateUnion).cs
    } catch {
      case SubtypeFailure() =>
        cs
      case UnionFailure() if !tolerateUnion =>
        cs
    }
  }

  @tailrec
  private def constrain(
      state: State,
      lower: Type,
      upper: Type,
      seen: Set[(Type, Type)],
      tolerateUnion: Boolean,
  ): State = {
    val State(toSolve, varsToElim, constraints, variances, constraintLoc) = state

    def failSubtype(): Nothing = {
      val cs = meetAllConstraints(constraints, variances, Map.empty)
      subtypeFailure(cs, constraintLoc, variances)
    }
    def failUnion(): Nothing = {
      unionFailure(constraintLoc, tolerateUnion)
    }
    if (!TypeVars.hasTypeVars(upper) && !TypeVars.hasTypeVars(lower)) state
    // The logic is similar to Subtype.scala
    else if (seen((lower, upper))) state
    else if (subtype.subType(lower, upper)) state
    else
      (lower, upper) match {
        // CG-Upper from Pierce and "Turner Local Type Inference"
        case (FreeVarType(n), _) if toSolve(n) =>
          assert(TypeVars.freeVars(upper).intersect(toSolve).isEmpty)
          val constraint = Constraint(NoneType, TypeVars.demote(upper, varsToElim))
          state.copy(cs = constraints :+ (n, constraintLoc, constraint))
        // CG-Lower
        case (_, FreeVarType(n)) if toSolve(n) =>
          assert(TypeVars.freeVars(lower).intersect(toSolve).isEmpty)
          val constraint = Constraint(TypeVars.promote(lower, varsToElim), AnyType)
          state.copy(cs = constraints :+ (n, constraintLoc, constraint))
        case (FreeVarType(n1), FreeVarType(n2)) if n1 == n2 && !toSolve(n1) =>
          state
        case (DynamicType, _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, constraintLoc, Constraint(DynamicType, AnyType))).toVector
          state.copy(cs = constraints ++ newConstraints)
        case (BoundedDynamicType(_), _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, constraintLoc, Constraint(DynamicType, AnyType))).toVector
          state.copy(cs = constraints ++ newConstraints)
        case (_, BoundedDynamicType(bound)) =>
          constrain(state, lower, bound, seen, tolerateUnion)
        // logic for recursive types is the same as in subtype.scala
        case (RemoteType(rid, args), _) =>
          constrain(
            state,
            util.getTypeDeclBody(rid, args),
            upper,
            seen + (lower -> upper),
            tolerateUnion,
          )
        case (_, RemoteType(rid, args)) =>
          constrain(
            state,
            lower,
            util.getTypeDeclBody(rid, args),
            seen + (lower -> upper),
            tolerateUnion,
          )

        // CG-Fun
        case (rawFt1: FunType, rawFt2: FunType) if rawFt1.argTys.size == rawFt2.argTys.size =>
          TypeVars.conformForalls(rawFt1, rawFt2) match {
            case Some((ft1, ft2)) =>
              assert(ft1.forall == ft2.forall)
              val bounds = ft2.argTys.zip(ft1.argTys) ++ List((ft1.resTy, ft2.resTy))
              constrainSeq(state, bounds, seen, tolerateUnion)
            case None =>
              failSubtype()
          }

        case (ft1: AnyArityFunType, ft2: FunType) if ft2.forall == 0 =>
          constrain(state, ft1.resTy, ft2.resTy, seen, tolerateUnion)

        case (ft1: FunType, ft2: AnyArityFunType) =>
          val (vars, ft11) = instantiate.instantiate(ft1)
          val st = state.copy(varsToElim = state.varsToElim ++ vars)
          constrain(st, ft11.resTy, ft2.resTy, seen, tolerateUnion)

        case (ft1: AnyArityFunType, ft2: AnyArityFunType) =>
          constrain(state, ft1.resTy, ft2.resTy, seen, tolerateUnion)

        case (UnionType(tys), _) =>
          constrainSeq(state, tys.map((_, upper)), seen, tolerateUnion)
        // when the upper bound is a union, see if there is only one potential match, use it for constraint generation
        case (_, UnionType(tys)) =>
          val elimmedLower = TypeVars.demote(lower, toSolve ++ varsToElim)
          val candidates = tys.filter { ty =>
            val elimmedUpper = TypeVars.promote(ty, toSolve ++ varsToElim)
            subtype.subType(elimmedLower, elimmedUpper)
          }.toList
          val (varTypes, others) = candidates.partition {
            case _: FreeVarType => true
            case _              => false
          }
          (varTypes, others) match {
            case (_, List(upperBound)) =>
              constrain(state, lower, upperBound, seen, tolerateUnion)
            case (List(upperBound), _) =>
              constrain(state, lower, upperBound, seen, tolerateUnion)
            case (List(), List()) =>
              failSubtype()
            case (_, _) =>
              failUnion()
          }
        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, recordAsTuple(recDecl), t, seen, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, t, recordAsTuple(recDecl), seen, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, refinedRecordAsTuple(recDecl, r), t, seen, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, t, refinedRecordAsTuple(recDecl, r), seen, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (r1: RefinedRecordType, r2: RefinedRecordType) =>
          if (r1.recType == r2.recType)
            util.getRecord(r1.recType.module, r1.recType.name) match {
              case Some(recDecl) =>
                constrain(
                  state,
                  refinedRecordAsTuple(recDecl, r1),
                  refinedRecordAsTuple(recDecl, r2),
                  seen,
                  tolerateUnion,
                )
              case None =>
                failSubtype()
            }
          else
            failSubtype()
        case (TupleType(leftTys), TupleType(rightTys)) if leftTys.size == rightTys.size =>
          constrainSeq(state, leftTys.zip(rightTys), seen, tolerateUnion)
        case (ListType(leftElemTy), ListType(rightElemTy)) =>
          constrain(state, leftElemTy, rightElemTy, seen, tolerateUnion)
        case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
          // adapted from subtype.subtype
          val tolerantSubtype = subtype.isDynamicType(kT1) && subtype.isDynamicType(vT1)
          var constraints: List[(Type, Type)] = List()
          val reqKeys1 = props1.collect { case (k, Prop(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, Prop(true, _)) => k }.toSet
          if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) failSubtype()
          for ((key1, prop1) <- props1) {
            props2.get(key1) match {
              case Some(prop2) =>
                constraints = (prop1.tp, prop2.tp) :: constraints
              case None =>
                constraints = (asType(key1), kT2) :: (prop1.tp, vT2) :: constraints
            }
          }
          val elimmedkT1 = TypeVars.promote(kT1, toSolve ++ varsToElim)
          for ((key2, prop2) <- props2.removedAll(props1.keySet)) {
            if (subtype.subType(asType(key2), elimmedkT1)) {
              constraints = (asType(key2), kT1) :: (vT1, prop2.tp) :: constraints
            }
          }
          constraints = (kT1, kT2) :: (vT1, vT2) :: constraints
          constrainSeq(state, constraints, seen, tolerateUnion)
        case _ =>
          failSubtype()
      }
  }

  private def subtypeFailure(
      cs: Map[Var, Constraint],
      cLoc: ConstraintLoc,
      variances: Map[Var, Variance],
  ): Nothing = {
    val subst = constraintsToSubst(cs, variances)
    val expected = Subst.subst(subst, cLoc.paramTy)
    diagnosticsInfo.add(ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy))
    throw SubtypeFailure()
  }

  private def unionFailure(
      cLoc: ConstraintLoc,
      tolerateUnion: Boolean,
  ): Nothing = {
    if (!tolerateUnion) {
      diagnosticsInfo.add(AmbiguousUnion(cLoc.arg.pos, cLoc.arg, cLoc.paramTy, got = cLoc.argTy))
    }
    throw UnionFailure()
  }

  private def constrainSeq(
      state: State,
      bounds: Iterable[(Type, Type)],
      seen: Set[(Type, Type)],
      tolerateUnion: Boolean,
  ): State =
    bounds.foldLeft(state) { case (state, (lower, upper)) => constrain(state, lower, upper, seen, tolerateUnion) }

  private def meetConstraints(
      constraints: Map[Var, Constraint],
      entry: (Var, ConstraintLoc, Constraint),
      variances: Map[Var, Variance],
  ): Map[Var, Constraint] = {
    val (tv, cLoc, c2) = entry
    constraints.get(tv) match {
      case None =>
        constraints + (tv -> c2)
      case Some(c1) =>
        val upper = meet(c1.upper, c2.upper)
        val lower = subtype.join(c1.lower, c2.lower)
        if (!subtype.subType(c2.lower, c1.upper)) {
          val subst = constraintsToSubst(constraints, variances) + (tv -> c1.upper)
          val expected = Subst.subst(subst, cLoc.paramTy)
          diagnosticsInfo.add(ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy))
          constraints + (tv -> Constraint(DynamicType, DynamicType))
        } else if (!subtype.subType(lower, upper)) {
          val subst = constraintsToSubst(constraints, variances) + (tv -> c1.lower)
          val expected = Subst.subst(subst, cLoc.paramTy)
          diagnosticsInfo.add(ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy))
          constraints + (tv -> Constraint(DynamicType, DynamicType))
        } else {
          constraints + (tv -> Constraint(lower, upper))
        }
    }
  }

  def meetAllConstraints(
      cs: ConstraintSeq,
      variances: Map[Var, Variance],
      constraints: Map[Var, Constraint],
  ): Map[Var, Constraint] =
    cs.foldLeft(constraints)(meetConstraints(_, _, variances))

  def constraintsSeqToSubst(cseq: ConstraintSeq, variances: Map[Var, Variance], toSolve: Set[Var]): Map[Var, Type] = {
    val meets = meetAllConstraints(cseq, variances, Map.empty)
    val map1 = constraintsToSubst(meets, variances)
    val map2 = (toSolve -- meets.keySet).map(_ -> DynamicType)
    map1 ++ map2
  }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance]): Map[Var, Type] =
    cs.map { case (tv, c) => tv -> constraintToType(c, variances(tv)) }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance], toSolve: Set[Var]): Map[Var, Type] = {
    val map1 = cs.map { case (tv, c) => tv -> constraintToType(c, variances(tv)) }
    val map2 = (toSolve -- cs.keySet).map(_ -> DynamicType)
    map1 ++ map2
  }

  private def constraintToType(c: Constraint, variance: Variance): Type = variance match {
    case Variance.Constant | Variance.Covariant | Variance.Invariant =>
      c.lower
    case Variance.Contravariant =>
      c.upper
  }

  /** Safe approximation because we re-check arg types once we have concrete param types
    */
  private def meet(t1: Type, t2: Type): Type = narrow.meet(t1, t2)
}
