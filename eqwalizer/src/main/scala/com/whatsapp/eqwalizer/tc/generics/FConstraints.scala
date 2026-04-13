/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.tc.PipelineContext

import scala.annotation.tailrec

object FConstraints {
  private type Var = Int

  case class Constraint(lower: Type, upper: Type)
  type CMap = Map[Var, Constraint]

  private case class State(toSolve: Set[Var], varsToElim: Set[Var])
}

// A functional version of Constraints.
// It's either a success or a failure, no "patching with dynamic" and no "on the fly error reporting".
// Used for checks for type containment in subtyping.
class FConstraints(pipelineContext: PipelineContext) {
  import FConstraints.*
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val instantiate = pipelineContext.instantiate
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  /** Check whether there exists an instantiation of `toSolve` variables that makes all
   * (lower, upper) pairs in `pairs` satisfy lower <: upper.
   * Used for type containment: forall T. body <: target if exists T s.t. body <: target.
   */
  def satisfiable(
      toSolve: Set[Var],
      varsToElim: Set[Var],
      pairs: List[(Type, Type)],
  ): Boolean = {
    val state = State(toSolve = toSolve, varsToElim = varsToElim)
    constrainSeq(state, pairs, seen = Set.empty) match {
      case None =>
        false
      case Some(cs) =>
        var meets: Option[CMap] = Some(Map.empty)
        for (c <- cs)
          if (meets.isDefined)
            meets = meets.flatMap(meetConstraints(_, c))
        meets.exists(_.values.forall(c => subtype.subType(c.lower, c.upper)))
    }
  }

  @tailrec
  private def constrain(
      state: State,
      lowerBound: Type,
      upperBound: Type,
      seen: Set[(Type, Type)],
  ): Option[List[CMap]] = {
    val State(toSolve, varsToElim) = state

    if (toSolve.isEmpty) Some(List(Map.empty))
    else if (!TypeVars.hasTypeVars(upperBound) && !TypeVars.hasTypeVars(lowerBound)) Some(List(Map.empty))
    // The logic is similar to Subtype.scala
    else if (seen((lowerBound, upperBound))) Some(List(Map.empty))
    else if (lowerBound == upperBound) Some(List(Map.empty))
    else if (subtype.isAnyType(upperBound)) Some(List(Map.empty))
    else if (subtype.isNoneType(lowerBound)) Some(List(Map.empty))
    else
      (lowerBound, upperBound) match {
        // CG-Upper from Pierce and "Turner Local Type Inference"
        case (FreeVarType(n), _) if toSolve(n) =>
          assert(freeVars(upperBound).intersect(toSolve).isEmpty)
          val upper = TypeVars.demote(upperBound, varsToElim)
          val constraint = Constraint(NoneType, upper)
          Some(List(Map(n -> constraint)))
        // CG-Lower
        case (_, FreeVarType(n)) if toSolve(n) =>
          assert(freeVars(lowerBound).intersect(toSolve).isEmpty)
          val lower = TypeVars.promote(lowerBound, varsToElim)
          val constraint = Constraint(lower, AnyType)
          Some(List(Map(n -> constraint)))
        case (FreeVarType(n1), FreeVarType(n2)) if n1 == n2 && !toSolve(n1) =>
          Some(List(Map.empty))
        case (DynamicType, _) =>
          val solveFor = freeVars(upperBound).intersect(toSolve)
          Some(List(solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toMap))
        case (BoundedDynamicType(_), _) =>
          val solveFor = freeVars(upperBound).intersect(toSolve)
          Some(List(solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toMap))
        case (_, BoundedDynamicType(bound)) =>
          constrain(state, lowerBound, bound, seen)
        // logic for recursive types is the same as in subtype.scala
        case (RemoteType(rid, args), _) =>
          val lower = util.getTypeDeclBody(rid, args)
          constrain(state, lower, upperBound, seen + (lowerBound -> upperBound))
        case (_, RemoteType(rid, args)) =>
          val upper = util.getTypeDeclBody(rid, args)
          constrain(state, lowerBound, upper, seen + (lowerBound -> upperBound))

        // CG-Fun
        case (rawFt1: FunType, rawFt2: FunType) if rawFt1.argTys.size == rawFt2.argTys.size =>
          TypeVars.conformForalls(rawFt1, rawFt2) match {
            case Some((ft1, ft2)) =>
              assert(ft1.forall == ft2.forall)
              val lowersAndUppers = ft2.argTys.zip(ft1.argTys) ++ List((ft1.resTy, ft2.resTy))
              constrainSeq(state, lowersAndUppers, seen)
            case None =>
              None
          }

        case (ft1: AnyArityFunType, ft2: FunType) if ft2.forall == 0 =>
          constrain(state, ft1.resTy, ft2.resTy, seen)

        case (ft1: FunType, ft2: AnyArityFunType) =>
          val (vars, ft11) = instantiate.instantiate(ft1)
          val st = state.copy(varsToElim = state.varsToElim ++ vars)
          constrain(st, ft11.resTy, ft2.resTy, seen)

        case (ft1: AnyArityFunType, ft2: AnyArityFunType) =>
          constrain(state, ft1.resTy, ft2.resTy, seen)

        case (UnionType(tys), _) =>
          constrainSeq(state, tys.map((_, upperBound)), seen)
        // when the upper bound is a union, see if there is only one potential match, use it for constraint generation
        case (_, UnionType(tys)) =>
          val elimmedLower = TypeVars.demote(lowerBound, toSolve ++ varsToElim)
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
              constrain(state, lowerBound, upperBound, seen)
            case (List(upperBound), _) =>
              constrain(state, lowerBound, upperBound, seen)
            case (List(), List()) =>
              None
            case (_, _) =>
              None
          }
        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, recordAsTuple(recDecl), t, seen)
            case None =>
              None
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, t, recordAsTuple(recDecl), seen)
            case None =>
              None
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, refinedRecordAsTuple(recDecl, r), t, seen)
            case None =>
              None
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, t, refinedRecordAsTuple(recDecl, r), seen)
            case None =>
              None
          }
        case (r1: RefinedRecordType, r2: RefinedRecordType) =>
          if (r1.recType == r2.recType)
            util.getRecord(r1.recType.module, r1.recType.name) match {
              case Some(recDecl) =>
                constrain(state, refinedRecordAsTuple(recDecl, r1), refinedRecordAsTuple(recDecl, r2), seen)
              case None =>
                None
            }
          else
            None
        case (TupleType(leftTys), TupleType(rightTys)) if leftTys.size == rightTys.size =>
          constrainSeq(state, leftTys.zip(rightTys), seen)
        case (ListType(leftElemTy), ListType(rightElemTy)) =>
          constrain(state, leftElemTy, rightElemTy, seen)
        case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
          // adapted from subtype.subtype
          var constraints: List[(Type, Type)] = List()
          val reqKeys1 = props1.collect { case (k, Prop(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, Prop(true, _)) => k }.toSet
          if (!reqKeys2.subsetOf(reqKeys1)) return None
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
          constrainSeq(state, constraints, seen)
        case _ =>
          if (!subtype.subType(lowerBound, upperBound)) None
          else Some(List(Map.empty))
      }
  }

  private def constrainSeq(
      state: State,
      lowersAndUppers: Iterable[(Type, Type)],
      seen: Set[(Type, Type)],
  ): Option[List[CMap]] = {
    var result: Option[List[CMap]] = Some(List(Map.empty))
    for ((lowerBound, upperBound) <- lowersAndUppers)
      if (result.isDefined) {
        constrain(state, lowerBound, upperBound, seen) match {
          case Some(cs) => result = result.flatMap(meetConstraints(_, cs))
          case None     => result = None
        }
      }
    result
  }

  private def meetConstraints(cmap: CMap, tuple: (Var, Constraint)): Option[CMap] = {
    val (tv, c2) = tuple
    cmap.get(tv) match {
      case None =>
        Some(cmap + (tv -> c2))
      case Some(c1) =>
        val upper = meet(c1.upper, c2.upper)
        val lower = subtype.join(c1.lower, c2.lower)
        if (!subtype.subType(c2.lower, c1.upper)) {
          None
        } else if (!subtype.subType(lower, upper)) {
          None
        } else {
          Some(cmap + (tv -> Constraint(lower, upper)))
        }
    }
  }

  private def meetConstraints(m1: CMap, m2: CMap): Option[CMap] = {
    var result: Option[CMap] = Some(m1)
    for (tuple <- m2)
      if (result.isDefined)
        result = result.flatMap(meetConstraints(_, tuple))
    result
  }

  private def meetConstraints(ms1: List[CMap], ms2: List[CMap]): Option[List[CMap]] = {
    val meets = for (m1 <- ms1; m2 <- ms2; meet <- meetConstraints(m1, m2)) yield meet
    if (meets.isEmpty)
      None
    else
      Some(meets)
  }

  private def freeVars(ty: Type): Set[Var] = ty match {
    case FreeVarType(n) =>
      Set(n)
    case _ =>
      TypeVars.children(ty).flatMap(freeVars).toSet
  }

  private def meet(t1: Type, t2: Type): Type = narrow.meet(t1, t2)
}
