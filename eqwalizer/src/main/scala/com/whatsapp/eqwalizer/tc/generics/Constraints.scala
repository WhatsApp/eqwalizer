/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.{TypeVars, Variance}
import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.PipelineContext

import scala.annotation.tailrec

object Constraints {
  private type Var = Int

  case class Constraint(lower: Type, upper: Type)

  type ConstraintSeq = Vector[(Var, Constraint)]

  private case class State(toSolve: Set[Var], varsToElim: Set[Var])
  case class UnionFailure() extends Exception
}

class Constraints(pipelineContext: PipelineContext) {
  import Constraints._
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val instantiate = pipelineContext.instantiate
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def constraintGen(
      toSolve: Set[Var],
      lower: Type,
      upper: Type,
      tolerateUnion: Boolean,
  ): Option[ConstraintSeq] = {
    val state = State(toSolve, Set.empty)
    constrain(state, lower, upper, Set.empty, tolerateUnion)
  }

  @tailrec
  private def constrain(
      state: State,
      lower: Type,
      upper: Type,
      seen: Set[(Type, Type)],
      tolerateUnion: Boolean,
  ): Option[ConstraintSeq] = {
    val State(toSolve, varsToElim) = state

    def failUnion(): None.type = {
      unionFailure(tolerateUnion)
    }
    // The logic is similar to Subtype.scala
    if (seen((lower, upper))) Some(Vector.empty)
    else if (subtype.subType(lower, upper)) Some(Vector.empty)
    else
      (lower, upper) match {
        // CG-Upper from Pierce and "Turner Local Type Inference"
        case (FreeVarType(n), _) if toSolve(n) =>
          assert(TypeVars.freeVars(upper).intersect(toSolve).isEmpty)
          val constraint = Constraint(NoneType, TypeVars.demote(upper, varsToElim))
          Some(Vector(n -> constraint))
        // CG-Lower
        case (_, FreeVarType(n)) if toSolve(n) =>
          assert(TypeVars.freeVars(lower).intersect(toSolve).isEmpty)
          val constraint = Constraint(TypeVars.promote(lower, varsToElim), AnyType)
          Some(Vector(n -> constraint))
        case (FreeVarType(n1), FreeVarType(n2)) if n1 == n2 && !toSolve(n1) =>
          Some(Vector.empty)
        case (DynamicType, _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toVector
          Some(newConstraints)
        case (BoundedDynamicType(_), _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          val newConstraints = solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toVector
          Some(newConstraints)
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
              None
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
              None
            case (_, _) =>
              failUnion()
          }
        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, recordAsTuple(recDecl), t, seen, tolerateUnion)
            case None =>
              None
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, t, recordAsTuple(recDecl), seen, tolerateUnion)
            case None =>
              None
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, refinedRecordAsTuple(recDecl, r), t, seen, tolerateUnion)
            case None =>
              None
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, t, refinedRecordAsTuple(recDecl, r), seen, tolerateUnion)
            case None =>
              None
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
                None
            }
          else
            None
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
          if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) return None
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
          None
      }
  }

  private def unionFailure(tolerateUnion: Boolean): None.type =
    if (tolerateUnion) throw UnionFailure()
    else None

  private def constrainSeq(
      state: State,
      bounds: Iterable[(Type, Type)],
      seen: Set[(Type, Type)],
      tolerateUnion: Boolean,
  ): Option[ConstraintSeq] = {
    var result: Option[ConstraintSeq] = Some(Vector.empty)
    for ((lower, upper) <- bounds) {
      result.match {
        case None =>
          ()
        case Some(cs) =>
          result = constrain(state, lower, upper, seen, tolerateUnion).map(cs ++ _)
      }
    }
    result
  }

  private def meetConstraints(
      constraints: Map[Var, Constraint],
      entry: (Var, Constraint),
  ): Option[Map[Var, Constraint]] = {
    val (tv, c2) = entry
    constraints.get(tv) match {
      case None =>
        Some(constraints + (tv -> c2))
      case Some(c1) =>
        val upper = meet(c1.upper, c2.upper)
        val lower = subtype.join(c1.lower, c2.lower)
        if (!subtype.subType(c2.lower, c1.upper)) {
          None
        } else if (!subtype.subType(lower, upper)) {
          None
        } else {
          Some(constraints + (tv -> Constraint(lower, upper)))
        }
    }
  }

  def meetAllConstraints(
      cs: ConstraintSeq,
      constraints: Map[Var, Constraint],
  ): Option[Map[Var, Constraint]] =
    cs.foldLeft(Option(constraints))((cs, x) => cs.flatMap(meetConstraints(_, x)))

  def constraintsSeqToSubst(
      cseq: ConstraintSeq,
      variances: Map[Var, Variance],
      toSolve: Set[Var],
  ): Option[Map[Var, Type]] =
    meetAllConstraints(cseq, Map.empty).map { meets =>
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
