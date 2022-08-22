/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc.generics
import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.tc.TcDiagnostics.ExpectedSubtype
import com.whatsapp.eqwalizer.tc.generics.Variance.{ConstantOrCovariant, Contravariant, Invariant, Variance}
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
}

class Constraints(pipelineContext: PipelineContext) {
  import Constraints._
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
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
  ): ConstraintSeq = {
    val state = State(
      toSolve = toSolve,
      varsToElim = Set.empty,
      cs,
      variances,
      seen = Set.empty,
      constraintLoc,
    )
    constrain(state, lowerBound, upperBound).cs
  }

  @tailrec
  private def constrain(state: State, lowerBound: Type, upperBound: Type): State = {
    val State(toSolve, varsToElim, constraints, variances, seen, constraintLoc) = state

    def fail(): Nothing = {
      val cs = meetAllConstraints(constraints, variances)
      failure(cs, constraintLoc, variances)
    }
    if (toSolve.isEmpty) state
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
        case (DynamicType, _) => state
        // logic for recursive types is the same as in subtype.scala
        case (RemoteType(rid, args), _) =>
          val lower = util.getTypeDeclBody(rid, args)
          constrain(state.copy(seen = seen + (lowerBound -> upperBound)), lower, upperBound)
        case (_, RemoteType(rid, args)) =>
          val upper = util.getTypeDeclBody(rid, args)
          constrain(state.copy(seen = seen + (lowerBound -> upperBound)), lowerBound, upper)

        // CG-Fun
        case (rawFt1: FunType, rawFt2: FunType) if rawFt1.argTys.size == rawFt2.argTys.size =>
          TypeVars.conformForalls(rawFt1, rawFt2) match {
            case Some((ft1, ft2)) =>
              assert(ft1.forall == ft2.forall)
              val st = state.copy(varsToElim = varsToElim ++ ft1.forall)
              val lowersAndUppers = ft2.argTys.zip(ft1.argTys) ++ List((ft1.resTy, ft2.resTy))
              constrainSeq(st, lowersAndUppers)
            case None =>
              fail()
          }

        case (UnionType(tys), _) =>
          constrainSeq(state, tys.map((_, upperBound)))
        // when the upper bound is a union with a ty var and there is only one potential match, use it for constraint generation
        case (_, UnionType(tys)) if TypeVars.hasTypeVars(upperBound) =>
          val candidates = tys.filter { ty =>
            val elimmedUpper = ElimTypeVars.elimTypeVars(ty, ElimTypeVars.Promote, toSolve ++ varsToElim)
            subtype.subType(lowerBound, elimmedUpper)
          }.toList
          val (varTypes, others) = candidates.partition {
            case _: VarType => true
            case _          => false
          }
          (varTypes, others) match {
            case (_, List(upperBound)) =>
              constrain(state, lowerBound, upperBound)
            case (List(upperBound), _) =>
              constrain(state, lowerBound, upperBound)
            case (_, _) =>
              fail()
          }
        case (r: RecordType, t: TupleType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, recordAsTuple(recDecl), t)
            case None =>
              fail()
          }
        case (t: TupleType, r: RecordType) =>
          util.getRecord(r.module, r.name) match {
            case Some(recDecl) =>
              constrain(state, t, recordAsTuple(recDecl))
            case None =>
              fail()
          }
        case (r: RefinedRecordType, t: TupleType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, refinedRecordAsTuple(recDecl, r), t)
            case None =>
              fail()
          }
        case (t: TupleType, r: RefinedRecordType) =>
          util.getRecord(r.recType.module, r.recType.name) match {
            case Some(recDecl) =>
              constrain(state, t, refinedRecordAsTuple(recDecl, r))
            case None =>
              fail()
          }
        case (r1: RefinedRecordType, r2: RefinedRecordType) =>
          if (r1.recType == r2.recType)
            util.getRecord(r1.recType.module, r1.recType.name) match {
              case Some(recDecl) =>
                constrain(state, refinedRecordAsTuple(recDecl, r1), refinedRecordAsTuple(recDecl, r2))
              case None =>
                fail()
            }
          else
            fail()
        case (TupleType(leftTys), TupleType(rightTys)) if leftTys.size == rightTys.size =>
          constrainSeq(state, leftTys.zip(rightTys))
        case (OpaqueType(id1, leftTys), OpaqueType(id2, rightTys)) if id1 == id2 =>
          constrainSeq(state, leftTys.zip(rightTys))
        case (ListType(leftElemTy), ListType(rightElemTy)) =>
          constrain(state, leftElemTy, rightElemTy)
        case (DictMap(leftKeyTy, leftValTy), DictMap(rKeyTy, rValTy)) =>
          constrainSeq(state, List((leftKeyTy, rKeyTy), (leftValTy, rValTy)))
        case (ShapeMap(props), DictMap(kT, vT)) =>
          // adapted from subtype.subtype
          val shapeDomain = subtype.join(props.map(prop => AtomLitType(prop.key)))
          val shapeCodomain = subtype.join(props.map(_.tp))
          constrainSeq(state, List((shapeDomain, kT), (shapeCodomain, vT)))
        case (ShapeMap(props1), ShapeMap(props2)) =>
          // adapted from subtype.subtype
          val keys1 = props1.map(_.key).toSet
          val keys2 = props2.map(_.key).toSet
          if (!keys1.subsetOf(keys2)) fail()
          val reqKeys1 = props1.collect { case ReqProp(k, _) => k }.toSet
          val reqKeys2 = props2.collect { case ReqProp(k, _) => k }.toSet
          if (!reqKeys2.subsetOf(reqKeys1)) fail()
          val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
          val uppersAndLowers = for {
            prop1 <- props1
            t1 = prop1.tp
            t2 = kvs2(prop1.key)
          } yield (t1, t2)
          constrainSeq(state, uppersAndLowers)
        case (
              _: AtomLitType | AnyFunType | AnyTupleType | NilType | _: RecordType | BinaryType | _: BuiltinType |
              _: TupleType | _: DictMap | _: ShapeMap | _: OpaqueType | _: VarType | _: ListType | _: FunType |
              _: RefinedRecordType,
              _,
            ) =>
          if (!subtype.subType(lowerBound, upperBound)) fail()
          else state
      }
  }

  private def failure(
      cs: Map[Var, Constraint],
      cLoc: ConstraintLoc,
      variances: Map[Var, Variance],
  ): Nothing = {
    val subst = constraintsToSubst(cs, variances)
    val expected = Subst.subst(subst, cLoc.paramTy)
    throw ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy)
  }

  private def constrainSeq(state0: State, lowersAndUppers: Iterable[(Type, Type)]): State =
    lowersAndUppers.foldLeft(state0) { case (state1, (lowerBound, upperBound)) =>
      constrain(state1, lowerBound, upperBound)
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
          throw ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy)
        }
        if (!subtype.subType(lower, upper)) {
          val subst = constraintsToSubst(constraints, variances) + (tv -> c1.lower)
          val expected = Subst.subst(subst, cLoc.paramTy)
          throw ExpectedSubtype(cLoc.arg.pos, cLoc.arg, expected, got = cLoc.argTy)
        }
        constraints + (tv -> Constraint(lower, upper))
    }
  }

  private def meetAllConstraints(
      cs: ConstraintSeq,
      variances: Map[Var, Variance],
  ): Map[Var, Constraint] = {
    cs.foldLeft(Map.empty[Var, Constraint])(meetConstraints(_, _, variances))
  }

  def constraintsSeqToSubst(cseq: ConstraintSeq, variances: Map[Var, Variance], toSolve: Set[Var]): Map[Var, Type] = {
    val meets = meetAllConstraints(cseq, variances)
    val cs = meets ++ (toSolve -- meets.keySet).map(_ -> emptyConstraint)
    constraintsToSubst(cs, variances)
  }

  private def constraintsToSubst(cs: Map[Var, Constraint], variances: Map[Var, Variance]): Map[Var, Type] =
    cs.map { case (tv, c) => tv -> constraintToType(c, variances(tv)) }

  def constraintToType(c: Constraint, variance: Variance): Type = variance match {
    case ConstantOrCovariant =>
      if (subtype.isNoneType(c.lower) && pipelineCtx.gradualTyping) DynamicType
      else c.lower
    case Invariant =>
      // Safe because we check all argument types against param types once we have a substitution.
      c.lower
    case Contravariant =>
      c.upper
  }

  /** Only used for asserts: can remove if performance becomes a problem.
    */
  private def freeVars(ty: Type): Set[Var] = freeVarsHelper(ty, Set.empty).toSet

  private def freeVarsHelper(ty: Type, bound: Set[Var]): List[Var] = ty match {
    case ft: FunType =>
      val bound1 = bound ++ ft.forall
      TypeVars.children(ft).flatMap(freeVarsHelper(_, bound1))
    case VarType(n) if !bound.contains(n) => Nil
    case _                                => TypeVars.children(ty).flatMap(freeVarsHelper(_, bound))
  }

  /** Safe approximation because we re-check arg types once we have concrete param types
    */
  private def meet(t1: Type, t2: Type): Type = narrow.meet(t1, t2)
}
