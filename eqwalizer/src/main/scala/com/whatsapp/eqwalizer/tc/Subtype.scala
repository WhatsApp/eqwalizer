/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types._

class Subtype(pipelineContext: PipelineContext) {
  val util = pipelineContext.util

  private sealed trait Variance
  private case object + extends Variance
  private case object - extends Variance

  private type Polarity = (Variance, Variance)

  private def negate(v: Variance): Variance =
    v match {
      case + => -
      case - => +
    }

  private def negate(polarity: Polarity): Polarity = {
    val (v1, v2) = polarity
    (negate(v2), negate(v1))
  }

  def subType(t1: Type, t2: Type): Boolean =
    subTypePol(t1, t2, Set.empty)(-, +)

  def gradualSubType(t1: Type, t2: Type): Boolean =
    subTypePol(t1, t2, Set.empty)(+, +) && subTypePol(t1, t2, Set.empty)(-, -)

  private def subTypePol(
      t1: Type,
      t2: Type,
      seen: Set[((Type, Variance), (Type, Variance))],
  )(implicit polarity: Polarity): Boolean = {
    val (v1, v2) = polarity
    (t1, t2) match {
      case (_, _) if seen(((t1, v1), (t2, v2))) =>
        true
      case (_, _) if t1 == t2 =>
        true

      case (_, AnyType) =>
        true
      case (NoneType, _) =>
        true
      case (DynamicType, _) if v1 == - =>
        true
      case (DynamicType, _) if v1 == + =>
        subTypePol(AnyType, t2, seen)
      case (_, DynamicType) if v2 == - =>
        subTypePol(t1, NoneType, seen)
      case (_, DynamicType) if v2 == + =>
        true
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        subTypePol(body, t2, seen + ((t1, v1) -> (t2, v2)))
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subTypePol(t1, body, seen + ((t1, v1) -> (t2, v2)))
      case (OpaqueType(id1, tys1), OpaqueType(id2, tys2)) =>
        id1 == id2 && tys1.lazyZip(tys2).forall(subTypePol(_, _, seen))

      case (UnionType(tys1), _) =>
        tys1.forall(subTypePol(_, t2, seen))

      case (ty1: TupleType, ty2: UnionType) if ty1.argTys.nonEmpty =>
        ty1.argTys.zipWithIndex.exists { case (elem, i) => subtypeTuple(elem, ty2, i, ty1, seen) }

      case (_, UnionType(tys2)) =>
        tys2.exists(subTypePol(t1, _, seen))

      case (AtomLitType(_), AtomType) =>
        true
      case (TupleType(_), AnyTupleType) =>
        true
      case (RecordType(_), AnyTupleType) =>
        true
      case (RefinedRecordType(_, _), AnyTupleType) =>
        true
      case (r: RecordType, t: TupleType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subTypePol(recordAsTuple(recDecl), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subTypePol(t, recordAsTuple(recDecl), seen)
          case None =>
            false
        }
      case (r: RefinedRecordType, t: TupleType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subTypePol(refinedRecordAsTuple(recDecl, r), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RefinedRecordType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subTypePol(t, refinedRecordAsTuple(recDecl, r), seen)
          case None =>
            false
        }
      case (refRec: RefinedRecordType, rec: RecordType) =>
        refRec.recType.name == rec.name
      case (rec: RecordType, refRec: RefinedRecordType) if rec == refRec.recType =>
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            refRec.fields.forall(f => subTypePol(recDecl.fields(f._1).tp, f._2, seen))
          case None =>
            false
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        util.getRecord(refRec1.recType.module, refRec1.recType.name) match {
          case None => false
          case Some(recDecl) =>
            refRec2.fields.forall { case (fName, fTy) =>
              if (refRec1.fields.contains(fName))
                subTypePol(refRec1.fields(fName), fTy, seen)
              else
                subTypePol(recDecl.fields(fName).tp, fTy, seen)
            }
        }
      case (AnyTupleType, TupleType(_)) if pipelineContext.gradualTyping =>
        true
      case (AnyTupleType, RecordType(_)) if pipelineContext.gradualTyping =>
        true
      case (AnyTupleType, RefinedRecordType(_, _)) if pipelineContext.gradualTyping =>
        true
      case (FunType(_, _, _), AnyFunType) if pipelineContext.gradualTyping =>
        true
      case (AnyFunType, FunType(_, _, _)) if pipelineContext.gradualTyping =>
        true
      case (FunType(_, argTys, _), AnyFunType) =>
        argTys.forall(subTypePol(AnyType, _, seen))
      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(subTypePol(_, _, seen))
      case (NilType, ListType(_)) =>
        true
      case (ListType(ty1), NilType) =>
        subTypePol(ty1, NoneType, seen)
      case (ListType(et1), ListType(et2)) =>
        subTypePol(et1, et2, seen)
      case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
        TypeVars.conformForalls(ft1, ft2) match {
          case None => false
          case Some((FunType(_, args1, res1), FunType(_, args2, res2))) =>
            subTypePol(res1, res2, seen) && args2
              .lazyZip(args1)
              .forall(subTypePol(_, _, seen)(negate(polarity)))
        }
      case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
        subTypePol(kT1, kT2, seen) && subTypePol(vT1, vT2, seen)
      case (ShapeMap(props), DictMap(kT, vT)) =>
        // keep this logic in sync with ElabGeneric.constraintGen
        val shapeDomain = join(props.map(prop => AtomLitType(prop.key)))
        val shapeCodomain = join(props.map(_.tp))
        subTypePol(shapeDomain, kT, seen) && subTypePol(shapeCodomain, vT, seen)
      case (ShapeMap(props1), ShapeMap(props2)) =>
        // keep this logic in sync with ElabGeneric.constraintGen
        val keys1 = props1.map(_.key).toSet
        val keys2 = props2.map(_.key).toSet
        if (!keys1.subsetOf(keys2)) return false
        val reqKeys1 = props1.collect { case ReqProp(k, _) => k }.toSet
        val reqKeys2 = props2.collect { case ReqProp(k, _) => k }.toSet
        if (!reqKeys2.subsetOf(reqKeys1)) return false
        val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
        for (prop1 <- props1) {
          val t1 = prop1.tp
          val t2 = kvs2(prop1.key)
          if (!subTypePol(t1, t2, seen)) return false
        }
        true
      case (DictMap(kT, vT), ShapeMap(_)) if isDynamicType(kT) && isDynamicType(vT) =>
        true
      case _ =>
        false
    }
  }

  def eqv(t1: Type, t2: Type): Boolean =
    subType(t1, t2) && subType(t2, t1)

  def isDynamicType(t: Type): Boolean =
    subType(t, NoneType) && subType(AnyType, t)

  def isNoneType(t: Type): Boolean =
    isNoneType(t, Set.empty)

  private def isNoneType(t: Type, seen: Set[Type]): Boolean =
    seen(t) || (t match {
      case NoneType =>
        true
      case UnionType(ts) =>
        ts.forall(isNoneType(_, seen))
      case OpaqueType(_, _) =>
        false
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        isNoneType(body, seen + t)
      case _ =>
        false
    })

  def isAnyType(t: Type): Boolean =
    isAnyType(t, Set.empty)

  private def isAnyType(t: Type, seen: Set[Type]): Boolean =
    seen(t) || (t match {
      case AnyType =>
        true
      case UnionType(ts) =>
        ts.exists(isAnyType(_, seen))
      case OpaqueType(_, _) =>
        false
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        isAnyType(body, seen + t)
      case _ =>
        false
    })

  private def hasDynamic(t: Type): Boolean =
    t match {
      case DynamicType =>
        true
      case UnionType(ts) =>
        ts.exists(hasDynamic)
      case RemoteType(rid, args) =>
        hasDynamic(util.getTypeDeclBody(rid, args))
      case _ =>
        false
    }

  private def staticAux(t: Type): Option[Type] =
    t match {
      case DynamicType =>
        None
      case UnionType(ts) =>
        val ts1 = ts.flatMap(staticAux)
        if (ts1.isEmpty) None else Some(UnionType(ts1))
      case RemoteType(rid, args) =>
        staticAux(util.getTypeDeclBody(rid, args))
      case _ =>
        Some(t)
    }

  private def static(t: Type): Type =
    staticAux(t).getOrElse(NoneType)

  /** Checks whether originalTuple.updated(proj, t1) < t2, by expanding t1 if it is an alias or a union.
    */
  private def subtypeTuple(
      t1: Type,
      t2: Type,
      proj: Int,
      originalTuple: TupleType,
      seen: Set[((Type, Variance), (Type, Variance))],
  )(implicit polarity: (Variance, Variance)): Boolean = {
    val (v1, v2) = polarity
    (t1, t2) match {
      // Standard cases from subType
      case (NoneType, _) =>
        true
      case (_, AnyType) =>
        true
      case (_, AnyTupleType) =>
        true
      case (_, DynamicType) if v2 == + =>
        true
      case (_, DynamicType) if v2 == - =>
        false
      case (DynamicType, _) if v1 == + =>
        subtypeTuple(AnyType, t2, proj, originalTuple, seen)
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(body, t2, proj, originalTuple, seen)

      // ** Main logic **
      case (UnionType(tys1), _) =>
        // Distributes a tuple of unions into a union of tuples
        tys1.forall(subtypeTuple(_, t2, proj, originalTuple, seen))
      case (_, TupleType(tys2)) if originalTuple.argTys.size == tys2.size =>
        // Injects the union back into the original tuple
        subTypePol(TupleType(originalTuple.argTys.updated(proj, t1)), t2, seen)
      // Standard cases from subType
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(t1, body, proj, originalTuple, seen + ((originalTuple, v1) -> (t2, v2)))
      case (_, UnionType(tys)) =>
        tys.exists(t => subtypeTuple(t1, t, proj, originalTuple, seen))
      case (_, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, recordAsTuple(recDecl), proj, originalTuple, seen)
          case None =>
            false
        }
      case (_, r: RefinedRecordType) =>
        val recTy = r.recType
        util.getRecord(recTy.module, recTy.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, refinedRecordAsTuple(recDecl, r), proj, originalTuple, seen)
          case None =>
            false
        }
      case _ =>
        false
    }
  }

  def joinEnvs(envs: List[Env]): Env = {
    val vars = envs.map(_.keySet).reduce(_.intersect(_))
    var envAcc: Env = envs.head.filter { case (k, _) => vars(k) }
    for { env <- envs; v <- vars } envAcc = envAcc.updated(v, join(envAcc(v), env(v)))
    envAcc
  }

  def join(ts: Iterable[Type]): Type =
    ts.fold(NoneType)(join)

  def join(t1: Type, t2: Type): Type = {
    val dynamic1 = hasDynamic(t1)
    val dynamic2 = hasDynamic(t2)

    val static1 = if (dynamic1) static(t1) else t1
    val static2 = if (dynamic2) static(t2) else t2

    val staticRes =
      if (gradualSubType(static1, static2)) static2
      else if (gradualSubType(static2, static1)) static1
      else UnionType(Set(static1, static2))

    if (dynamic1 || dynamic2) {
      if (staticRes == NoneType) DynamicType else UnionType(Set(DynamicType, staticRes))
    } else
      staticRes
  }
}
