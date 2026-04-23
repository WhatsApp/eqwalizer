/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Subst.Subst
import com.whatsapp.eqwalizer.ast.{TypeVars, Variance}
import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.PipelineContext

object Constraints {
  private type Var = Int

  case class Constraint(lower: Type, upper: Type) {
    def toType(variance: Variance): Type = variance match {
      case Variance.Constant | Variance.Covariant | Variance.Invariant =>
        lower
      case Variance.Contravariant =>
        upper
    }
  }
  type CMap = Map[Var, Constraint]

  private case class Ctx(toSolve: Set[Var], varsToElim: Set[Var])
}

class Constraints(pipelineContext: PipelineContext) {
  import Constraints._
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val instantiate = pipelineContext.instantiate
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def satisfiable(toSolve: Set[Var], varsToElim: Set[Var], bounds: List[(Type, Type)]): Boolean = {
    val ctx = Ctx(toSolve = toSolve, varsToElim = varsToElim)
    constrainSeq(ctx, bounds, seen = Set.empty) match {
      case None =>
        false
      case Some(cs) =>
        var meets: Option[CMap] = Some(Map.empty)
        for (c <- cs; if meets.isDefined)
          meets = meets.flatMap(meetConstraints(_, c))
        meets.exists(_.values.forall(c => subtype.subType(c.lower, c.upper)))
    }
  }

  def constraintGen(toSolve: Set[Var], lower: Type, upper: Type): Option[List[CMap]] = {
    val state = Ctx(toSolve, Set.empty)
    constrain(state, lower, upper, Set.empty)
  }

  private def constrain(ctx: Ctx, lower: Type, upper: Type, seen: Set[(Type, Type)]): Option[List[CMap]] = {
    val Ctx(toSolve, varsToElim) = ctx

    // The logic is similar to Subtype.scala
    if (seen((lower, upper))) Some(List(Map.empty))
    else if (subtype.subType(lower, upper)) Some(List(Map.empty))
    else
      (lower, upper) match {
        // CG-Upper from Pierce and "Turner Local Type Inference"
        case (FreeVarType(n), _) if toSolve(n) =>
          assert(TypeVars.freeVars(upper).intersect(toSolve).isEmpty)
          val constraint = Constraint(NoneType, TypeVars.demote(upper, varsToElim))
          Some(List(Map(n -> constraint)))
        // CG-Lower
        case (_, FreeVarType(n)) if toSolve(n) =>
          assert(TypeVars.freeVars(lower).intersect(toSolve).isEmpty)
          val constraint = Constraint(TypeVars.promote(lower, varsToElim), AnyType)
          Some(List(Map(n -> constraint)))
        case (FreeVarType(n1), FreeVarType(n2)) if n1 == n2 && !toSolve(n1) =>
          Some(List(Map.empty))
        case (DynamicType, _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          Some(List(solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toMap))
        case (BoundedDynamicType(_), _) =>
          val solveFor = TypeVars.freeVars(upper).intersect(toSolve)
          Some(List(solveFor.map(n => (n, Constraint(DynamicType, AnyType))).toMap))
        case (_, BoundedDynamicType(bound)) =>
          constrain(ctx, lower, bound, seen)
        // logic for recursive types is the same as in subtype.scala
        case (RemoteType(rid, args), _) =>
          constrain(ctx, util.getTypeDeclBody(rid, args), upper, seen + (lower -> upper))
        case (_, RemoteType(rid, args)) =>
          constrain(ctx, lower, util.getTypeDeclBody(rid, args), seen + (lower -> upper))

        // CG-Fun
        case (rawFt1: FunType, rawFt2: FunType) if rawFt1.argTys.size == rawFt2.argTys.size =>
          TypeVars.conformForalls(rawFt1, rawFt2) match {
            case Some((ft1, ft2)) =>
              assert(ft1.forall == ft2.forall)
              val bounds = ft2.argTys.zip(ft1.argTys) ++ List((ft1.resTy, ft2.resTy))
              constrainSeq(ctx, bounds, seen)
            case None =>
              None
          }
        case (ft1: AnyArityFunType, ft2: FunType) if ft2.forall == 0 =>
          constrain(ctx, ft1.resTy, ft2.resTy, seen)
        case (ft1: FunType, ft2: AnyArityFunType) =>
          val (vars, ft11) = instantiate.instantiate(ft1)
          val st = ctx.copy(varsToElim = ctx.varsToElim ++ vars)
          constrain(st, ft11.resTy, ft2.resTy, seen)
        case (ft1: AnyArityFunType, ft2: AnyArityFunType) =>
          constrain(ctx, ft1.resTy, ft2.resTy, seen)

        case (UnionType(tys), _) =>
          constrainSeq(ctx, tys.map((_, upper)), seen)
        case (_, UnionType(tys)) =>
          val results = tys.flatMap(constrain(ctx, lower, _, seen)).toList
          if (results.isEmpty)
            None
          else
            Some(results.flatten)

        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(ctx, recordAsTuple(recDecl), t, seen)
            case None =>
              None
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(ctx, t, recordAsTuple(recDecl), seen)
            case None =>
              None
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(ctx, refinedRecordAsTuple(recDecl, r), t, seen)
            case None =>
              None
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(ctx, t, refinedRecordAsTuple(recDecl, r), seen)
            case None =>
              None
          }
        case (r1: RefinedRecordType, r2: RefinedRecordType) =>
          if (r1.recType == r2.recType)
            util.getRecord(r1.recType.module, r1.recType.name) match {
              case Some(recDecl) =>
                constrain(ctx, refinedRecordAsTuple(recDecl, r1), refinedRecordAsTuple(recDecl, r2), seen)
              case None =>
                None
            }
          else
            None
        case (TupleType(leftTys), TupleType(rightTys)) if leftTys.size == rightTys.size =>
          constrainSeq(ctx, leftTys.zip(rightTys), seen)
        case (ListType(leftElemTy), ListType(rightElemTy)) =>
          constrain(ctx, leftElemTy, rightElemTy, seen)
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
          constrainSeq(ctx, constraints, seen)
        case _ =>
          None
      }
  }

  private def constrainSeq(ctx: Ctx, bounds: Iterable[(Type, Type)], seen: Set[(Type, Type)]): Option[List[CMap]] = {
    var result: Option[List[CMap]] = Some(List(Map.empty))
    for ((lower, upper) <- bounds; if result.isDefined)
      constrain(ctx, lower, upper, seen) match {
        case Some(cs) => result = result.flatMap(meetConstraints(_, cs))
        case None     => result = None
      }
    result
  }

  private def meetConstraints(cmap: CMap, entry: (Var, Constraint)): Option[CMap] = {
    val (tv, c2) = entry
    cmap.get(tv) match {
      case None =>
        Some(cmap + entry)
      case Some(c1) =>
        if (!subtype.subType(c1.lower, c2.upper) || !subtype.subType(c2.lower, c1.upper)) None
        else Some(cmap + (tv -> Constraint(subtype.join(c1.lower, c2.lower), meet(c1.upper, c2.upper))))
    }
  }

  def meetConstraints(m1: CMap, m2: CMap): Option[CMap] = {
    var result: Option[CMap] = Some(m1)
    for (entry <- m2; if result.isDefined)
      result = result.flatMap(meetConstraints(_, entry))
    result
  }

  def meetConstraints(ms1: List[CMap], ms2: List[CMap]): Option[List[CMap]] = {
    val meets = for (m1 <- ms1; m2 <- ms2; meet <- meetConstraints(m1, m2)) yield meet
    if (meets.isEmpty) None else Some(meets)
  }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance]): Subst =
    cs.map { case (tv, c) => tv -> c.toType(variances(tv)) }

  def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance], toSolve: Set[Var]): Subst = {
    val map1 = cs.map { case (tv, c) => tv -> c.toType(variances(tv)) }
    val map2 = (toSolve -- cs.keySet).map(_ -> DynamicType)
    map1 ++ map2
  }

  /** Safe approximation because we re-check arg types once we have concrete param types
    */
  private def meet(t1: Type, t2: Type): Type = narrow.meet(t1, t2)
}
