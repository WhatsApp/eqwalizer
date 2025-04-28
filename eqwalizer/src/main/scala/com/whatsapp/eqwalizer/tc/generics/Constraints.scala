/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics
import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{AmbiguousUnion, ExpectedSubtype}
import com.whatsapp.eqwalizer.tc.generics.Variance._
import com.whatsapp.eqwalizer.tc.{PipelineContext, Subst}

import scala.annotation.tailrec

object Constraints {
  private type Var = Int

  trait ConstraintLoc {
    val arg: Expr
    val argTy: Type
    val paramTy: Type
  }

  case class Constraint(lower: Type, upper: Type)
  val emptyConstraint = Constraint(NoneType, AnyType)

  type ConstraintSeq = Vector[(Var, ConstraintLoc, Constraint)]

  private case class State(
      // `toSolve` is Xbar in Pierce and Turner "Local Type Inference" section 3.3: variables we are solving for
      toSolve: Set[Var],
      // varsToElim is set V in Pierce and Turner section 3.3: variables that should not appear in constraints (bound variables).
      // currently will *always* be empty because currently in EqWAlizer parameters with function types always have empty foralls (we don't have higher-rank types).
      varsToElim: Set[Var],
      cs: ConstraintSeq,
      variances: Map[Var, Variance],
      seen: Set[(Type, Type)], // for handling recursive types, same logic as in subtype.scala
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
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  /** Pierce and Turner Local Type Inference section 3.3
    * We're constraining `lowerBound` to be a subtype of `upperBound` (usually written `S &lt;: T` in the paper).
    *
    * Differences from the paper:
    * - We have more types of types in our system (tuples, for example).
    * - P&T don't handle pattern-matching. We use None to represent failure.
    * - We return early if there are no type variables to solve: we use same logic for both generic and non-generic fun application
    */
  def constraintGen(
      toSolve: Set[Var],
      lowerBound: Type,
      upperBound: Type,
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
      seen = Set.empty,
      constraintLoc,
    )
    try {
      constrain(state, lowerBound, upperBound, tolerateUnion).cs
    } catch {
      case SubtypeFailure() =>
        cs
      case UnionFailure() if !tolerateUnion =>
        cs
    }
  }

  @tailrec
  private def constrain(state: State, lowerBound: Type, upperBound: Type, tolerateUnion: Boolean): State = {
    val State(toSolve, varsToElim, constraints, variances, seen, constraintLoc) = state

    def failSubtype(): Nothing = {
      val cs = meetAllConstraints(constraints, variances, Map.empty)
      subtypeFailure(cs, constraintLoc, variances)
    }
    def failUnion(): Nothing = {
      unionFailure(constraintLoc, tolerateUnion)
    }
    if (toSolve.isEmpty) state
    else if (!TypeVars.hasTypeVars(upperBound) && !TypeVars.hasTypeVars(lowerBound)) state
    // The logic is similar to Subtype.scala
    else if (seen((lowerBound, upperBound))) state
    else if (lowerBound == upperBound) state
    else if (subtype.isAnyType(upperBound)) state
    else if (subtype.isNoneType(lowerBound)) state
    else
      (lowerBound, upperBound) match {
        // CG-Upper from Pierce and "Turner Local Type Inference"
        case (VarType(n), _) if toSolve(n) =>
          assert(freeVars(upperBound).intersect(toSolve).isEmpty)
          val upper = ElimTypeVars.elimTypeVars(upperBound, ElimTypeVars.Demote, varsToElim)
          val constraint = Constraint(NoneType, upper)
          state.copy(cs = constraints :+ (n, constraintLoc, constraint))
        // CG-Lower
        case (_, VarType(n)) if toSolve(n) =>
          assert(freeVars(lowerBound).intersect(toSolve).isEmpty)
          val lower = ElimTypeVars.elimTypeVars(lowerBound, ElimTypeVars.Promote, varsToElim)
          val constraint = Constraint(lower, AnyType)
          state.copy(cs = constraints :+ (n, constraintLoc, constraint))
        case (VarType(n1), VarType(n2)) if n1 == n2 && !toSolve(n1) =>
          state
        case (DynamicType, _) =>
          val solveFor = freeVars(upperBound).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, constraintLoc, Constraint(DynamicType, AnyType))).toVector
          state.copy(cs = constraints ++ newConstraints)
        case (BoundedDynamicType(_), _) =>
          val solveFor = freeVars(upperBound).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, constraintLoc, Constraint(DynamicType, AnyType))).toVector
          state.copy(cs = constraints ++ newConstraints)
        case (_, BoundedDynamicType(bound)) =>
          constrain(state, lowerBound, bound, tolerateUnion)
        // logic for recursive types is the same as in subtype.scala
        case (RemoteType(rid, args), _) =>
          val lower = util.getTypeDeclBody(rid, args)
          constrain(state.copy(seen = seen + (lowerBound -> upperBound)), lower, upperBound, tolerateUnion)
        case (_, RemoteType(rid, args)) =>
          val upper = util.getTypeDeclBody(rid, args)
          constrain(state.copy(seen = seen + (lowerBound -> upperBound)), lowerBound, upper, tolerateUnion)

        // CG-Fun
        case (rawFt1: FunType, rawFt2: FunType) if rawFt1.argTys.size == rawFt2.argTys.size =>
          TypeVars.conformForalls(rawFt1, rawFt2) match {
            case Some((ft1, ft2)) =>
              assert(ft1.forall == ft2.forall)
              val st = state.copy(varsToElim = varsToElim ++ ft1.forall)
              val lowersAndUppers = ft2.argTys.zip(ft1.argTys) ++ List((ft1.resTy, ft2.resTy))
              constrainSeq(st, lowersAndUppers, tolerateUnion)
            case None =>
              failSubtype()
          }

        case (ft1: AnyArityFunType, ft2: FunType) =>
          val st = state.copy(varsToElim = varsToElim ++ ft2.forall)
          constrain(st, ft1.resTy, ft2.resTy, tolerateUnion)

        case (ft1: FunType, ft2: AnyArityFunType) =>
          val st = state.copy(varsToElim = varsToElim ++ ft1.forall)
          constrain(st, ft1.resTy, ft2.resTy, tolerateUnion)

        case (ft1: AnyArityFunType, ft2: AnyArityFunType) =>
          constrain(state, ft1.resTy, ft2.resTy, tolerateUnion)

        case (UnionType(tys), _) =>
          constrainSeq(state, tys.map((_, upperBound)), tolerateUnion)
        // when the upper bound is a union, see if there is only one potential match, use it for constraint generation
        case (_, UnionType(tys)) =>
          val elimmedLower = ElimTypeVars.elimTypeVars(lowerBound, ElimTypeVars.Demote, toSolve ++ varsToElim)
          val candidates = tys.filter { ty =>
            val elimmedUpper = ElimTypeVars.elimTypeVars(ty, ElimTypeVars.Promote, toSolve ++ varsToElim)
            subtype.subType(elimmedLower, elimmedUpper)
          }.toList
          val (varTypes, others) = candidates.partition {
            case _: VarType => true
            case _          => false
          }
          (varTypes, others) match {
            case (_, List(upperBound)) =>
              constrain(state, lowerBound, upperBound, tolerateUnion)
            case (List(upperBound), _) =>
              constrain(state, lowerBound, upperBound, tolerateUnion)
            case (List(), List()) =>
              failSubtype()
            case (_, _) =>
              failUnion()
          }
        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, recordAsTuple(recDecl), t, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, t, recordAsTuple(recDecl), tolerateUnion)
            case None =>
              failSubtype()
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, refinedRecordAsTuple(recDecl, r), t, tolerateUnion)
            case None =>
              failSubtype()
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, t, refinedRecordAsTuple(recDecl, r), tolerateUnion)
            case None =>
              failSubtype()
          }
        case (r1: RefinedRecordType, r2: RefinedRecordType) =>
          if (r1.recType == r2.recType)
            util.getRecord(r1.recType.module, r1.recType.name) match {
              case Some(recDecl) =>
                constrain(state, refinedRecordAsTuple(recDecl, r1), refinedRecordAsTuple(recDecl, r2), tolerateUnion)
              case None =>
                failSubtype()
            }
          else
            failSubtype()
        case (TupleType(leftTys), TupleType(rightTys)) if leftTys.size == rightTys.size =>
          constrainSeq(state, leftTys.zip(rightTys), tolerateUnion)
        case (ListType(leftElemTy), ListType(rightElemTy)) =>
          constrain(state, leftElemTy, rightElemTy, tolerateUnion)
        case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
          // adapted from subtype.subtype
          var constraints: List[(Type, Type)] = List()
          val reqKeys1 = props1.collect { case (k, Prop(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, Prop(true, _)) => k }.toSet
          if (!reqKeys2.subsetOf(reqKeys1)) failSubtype()
          for ((key1, prop1) <- props1) {
            props2.get(key1) match {
              case Some(prop2) =>
                constraints = (prop1.tp, prop2.tp) :: constraints
              case None =>
                constraints = (asType(key1), kT2) :: (prop1.tp, vT2) :: constraints
            }
          }
          val elimmedkT1 = ElimTypeVars.elimTypeVars(kT1, ElimTypeVars.Promote, toSolve ++ varsToElim)
          for ((key2, prop2) <- props2.removedAll(props1.keySet)) {
            if (subtype.subType(asType(key2), elimmedkT1)) {
              constraints = (asType(key2), kT1) :: (vT1, prop2.tp) :: constraints
            }
          }
          constraints = (kT1, kT2) :: (vT1, vT2) :: constraints
          constrainSeq(state, constraints, tolerateUnion)
        case _ =>
          if (!subtype.subType(lowerBound, upperBound)) failSubtype()
          else state
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

  private def constrainSeq(state0: State, lowersAndUppers: Iterable[(Type, Type)], tolerateUnion: Boolean): State =
    lowersAndUppers.foldLeft(state0) { case (state1, (lowerBound, upperBound)) =>
      constrain(state1, lowerBound, upperBound, tolerateUnion)
    }

  private def meetConstraints(
      constraints: Map[Var, Constraint],
      triple: (Var, ConstraintLoc, Constraint),
      variances: Map[Var, Variance],
  ): Map[Var, Constraint] = {
    val (tv, cLoc, c2) = triple
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
  ): Map[Var, Constraint] = {
    cs.foldLeft(constraints)(meetConstraints(_, _, variances))
  }

  def constraintsSeqToSubst(cseq: ConstraintSeq, variances: Map[Var, Variance], toSolve: Set[Var]): Map[Var, Type] = {
    val meets = meetAllConstraints(cseq, variances, Map.empty)
    val cs = meets ++ (toSolve -- meets.keySet).map(_ -> emptyConstraint)
    constraintsToSubst(cs, variances)
  }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance]): Map[Var, Type] =
    cs.map { case (tv, c) => tv -> constraintToType(c, variances(tv)) }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance], toSolve: Set[Var]): Map[Var, Type] = {
    val cs1 = cs ++ (toSolve -- cs.keySet).map(_ -> emptyConstraint)
    cs1.map { case (tv, c) => tv -> constraintToType(c, variances(tv)) }
  }

  private def constraintToType(c: Constraint, variance: Variance): Type = variance match {
    case Constant | Covariant =>
      if (subtype.isNoneType(c.lower)) DynamicType
      else c.lower
    case Invariant =>
      // Safe because we check all argument types against param types once we have a substitution.
      if (subtype.isNoneType(c.lower)) DynamicType
      else c.lower
    case Contravariant =>
      c.upper
  }

  private def freeVars(ty: Type): Set[Var] = freeVarsHelper(ty, Set.empty).toSet

  private def freeVarsHelper(ty: Type, bound: Set[Var]): List[Var] = ty match {
    case ft: FunType =>
      val bound1 = bound ++ ft.forall
      TypeVars.children(ft).flatMap(freeVarsHelper(_, bound1))
    case VarType(n) if !bound.contains(n) => List(n)
    case VarType(_)                       => Nil
    case _                                => TypeVars.children(ty).flatMap(freeVarsHelper(_, bound))
  }

  /** Safe approximation because we re-check arg types once we have concrete param types
    */
  private def meet(t1: Type, t2: Type): Type = narrow.meet(t1, t2)
}
