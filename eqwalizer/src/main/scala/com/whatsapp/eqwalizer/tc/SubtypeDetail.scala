/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{RemoteId, Show, TypeVars}
import scala.util.boundary

object SubtypeDetail {
  lazy val nonVerboseRids = builtinTypes.keys.map(RemoteId("erlang", _, 0)).toSet

  private case class Detail(t1: Type, t2: Type, reasonPrefix: Option[String], reasonPostfix: Option[String])
}

class SubtypeDetail(pipelineContext: PipelineContext) {
  import SubtypeDetail._
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype

  def explain(got: Type, expected: Type): Option[String] =
    findSubtypeMismatch(got, expected) match {
      case Nil =>
        None // put debug assert here in T120664791, we expect to be able to produce an error explanation
      case List(Detail(_, _, None, None)) =>
        None
      case List(Detail(_, _, _, Some(reason))) =>
        Some(reason)
      case stack =>
        Some(
          stack
            .map { case Detail(t1, t2, reasonPrefix, reasonPostfix) =>
              val (showT1, showT2) = Show.showNotSubtype(t1, t2)(pipelineContext)
              val msgBefore = reasonPrefix.map(msg => s"  $msg\n").getOrElse("")
              val msgAfter = reasonPostfix.map(msg => s"\n  $msg").getOrElse("")
              s"$msgBefore  $showT1 is not compatible with $showT2$msgAfter"
            }
            .mkString("\n  because\n")
        )
    }

  private def findSubtypeMismatch(t1: Type, t2: Type): List[Detail] =
    findMismatchAux(t1, t2, Nil, Set.empty).reverse.take(pipelineContext.errorDepth)

  // keep this function in sync with subtype.subtype
  // Main differences from subtype.subtype's cases:
  // - instead of `false`, return stack
  // - instead of `true`, return Nil
  private def findMismatchAux(
      t1: Type,
      t2: Type,
      stack0: List[Detail],
      seen: Set[(Type, Type)],
  ): List[Detail] = {
    val stack = Detail(t1, t2, None, None) :: stack0

    @inline
    def recur(t1: Type, t2: Type): List[Detail] =
      findMismatchAux(t1, t2, stack, seen)

    @inline
    def recurSeq(pairs: List[(Type, Type)]): List[Detail] = {
      val stacks = pairs.iterator.map { case (t1, t2) => findMismatchAux(t1, t2, stack, seen) }
      stacks.find(_.nonEmpty).getOrElse(Nil)
    }

    (t1, t2) match {
      case (_, _) if seen((t1, t2)) =>
        Nil
      case (_, _) if t1 == t2 =>
        Nil

      case (_, AnyType) =>
        Nil
      case (NoneType, _) =>
        Nil

      case (DynamicType, _) =>
        Nil
      case (_, DynamicType) =>
        Nil

      case (BoundedDynamicType(_), _) =>
        Nil
      case (_, BoundedDynamicType(bound)) =>
        recur(t1, bound)

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        val stack1 = findMismatchAux(body, t2, stack, seen + (t1 -> t2))
        if (nonVerboseRids(rid) && stack1.nonEmpty) stack
        else stack1
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        val stack1 = findMismatchAux(t1, body, stack, seen + (t1 -> t2))
        if (nonVerboseRids(rid) && stack1.nonEmpty) stack
        else stack1

      case (ut: UnionType, _) =>
        val tys1 = util.flattenUnions(ut)
        val stacks = tys1.map(recur(_, t2))
        val noBranchMatches = stacks.forall(_.nonEmpty)
        if (noBranchMatches) stack
        else stacks.find(_.nonEmpty).getOrElse(Nil)
      case (_, ut: UnionType) =>
        val tys2 = util.flattenUnions(ut)
        if (subtype.subType(t1, ut))
          return Nil
        findCandidateInTys(t1, tys2) match {
          case Left(candidate) => recur(t1, candidate)
          case Right(reason) =>
            Detail(t1, t2, None, Some(reason)) :: stack0
        }
      case (AtomLitType(_), AtomType) => Nil
      case (TupleType(_), AnyTupleType) =>
        Nil
      case (FunType(_, argTys, _), AnyFunType) =>
        if (recurSeq(argTys.map((AnyType, _))).nonEmpty) stack
        else Nil
      case (AnyArityFunType(_), AnyFunType) =>
        Nil
      case (AnyFunType, AnyArityFunType(_)) =>
        Nil
      case (FunType(_, _, resTy1), AnyArityFunType(resTy2)) =>
        recur(resTy1, resTy2)
      case (AnyArityFunType(resTy1), FunType(_, _, resTy2)) =>
        recur(resTy1, resTy2)
      case (RecordType(_), AnyTupleType) =>
        Nil
      case (RefinedRecordType(_, _), AnyTupleType) =>
        Nil
      case (r: RecordType, t: TupleType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            findMismatchAux(recordAsTuple(recDecl), t, stack0, seen)
          case None =>
            stack
        }
      case (t: TupleType, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            findMismatchAux(t, recordAsTuple(recDecl), stack0, seen)
          case None =>
            stack
        }
      case (r: RefinedRecordType, t: TupleType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            findMismatchAux(refinedRecordAsTuple(recDecl, r), t, stack0, seen)
          case None =>
            stack
        }
      case (t: TupleType, r: RefinedRecordType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            findMismatchAux(t, refinedRecordAsTuple(recDecl, r), stack0, seen)
          case None =>
            stack
        }
      case (refRec: RefinedRecordType, rec: RecordType) if refRec.recType.name == rec.name =>
        Nil
      case (rec: RecordType, refRec: RefinedRecordType) if refRec.recType.name == rec.name =>
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            recurSeq(refRec.fields.toList.map(f => (recDecl.fMap(f._1).tp, f._2)))
          case None =>
            stack
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        util.getRecord(refRec1.recType.module, refRec1.recType.name) match {
          case Some(recDecl) =>
            recurSeq(refRec2.fields.collect {
              case (fName, fTy) if refRec1.fields.contains(fName) => (refRec1.fields(fName), fTy)
              case (fName, fTy)                                   => (recDecl.fMap(fName).tp, fTy)
            }.toList)
          case None =>
            stack
        }
      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        boundary {
          for (((itemTy1, indexFrom0), itemTy2) <- tys1.zipWithIndex.lazyZip(tys2)) {
            val index = indexFrom0 + 1
            val reasonPrefix = s"at tuple index $index:"
            val stackWithReason = Detail(t1, t2, Some(reasonPrefix), None) :: stack0
            findMismatchAux(itemTy1, itemTy2, stackWithReason, seen) match {
              case Nil     => ()
              case details => boundary.break(details)
            }
          }
          Nil
        }
      case (NilType, ListType(_)) =>
        Nil
      case (ListType(ty1), NilType) =>
        if (recur(ty1, NoneType).nonEmpty) stack
        else Nil
      case (ListType(et1), ListType(et2)) =>
        recur(et1, et2)
      case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
        TypeVars.conformForalls(ft1, ft2) match {
          case None => stack
          case Some((FunType(_, args1, res1), FunType(_, args2, res2))) =>
            recurSeq((res1, res2) :: args2.zip(args1))
        }
      case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
        boundary {
          val tolerantSubtype = subtype.isDynamicType(kT1) && subtype.isDynamicType(vT1)
          // Required keys don't match
          val reqKeys1 = props1.collect { case (k, Prop(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, Prop(true, _)) => k }.toSet
          if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) {
            val missingReqKeys = reqKeys2.removedAll(reqKeys1).map { s => s"`$s`" }.toList.sorted
            val reasonPostfix =
              if (missingReqKeys.size == 1)
                s"key ${missingReqKeys.head} is declared as required in the latter but not in the former"
              else
                s"keys ${missingReqKeys.mkString(", ")} are declared as required in the latter but not in the former"
            return Detail(t1, t2, None, Some(reasonPostfix)) :: stack0
          }
          // A key in the LHS doesn't match the RHS
          for ((key1, prop1) <- props1) {
            props2.get(key1) match {
              case Some(prop2) =>
                val reasonPrefix = s"at key `${key1}`:"
                val stackWithReason = Detail(t1, t2, Some(reasonPrefix), None) :: stack
                findMismatchAux(prop1.tp, prop2.tp, stackWithReason, seen) match {
                  case Nil     => ()
                  case details => boundary.break(details)
                }
              case None =>
                val reasonBegin = s"key `${key1}` is declared in the former but not in the latter"
                if (kT2 == NoneType) {
                  val reasonEnd = " and the latter map has no default association"
                  boundary.break(Detail(t1, t2, None, Some(reasonBegin ++ reasonEnd)) :: stack0)
                } else {
                  val reasonEnd =
                    s" and key `${key1}` isn't compatible with the default association of the latter map"
                  val stackWithReason = Detail(t1, t2, None, Some(reasonBegin ++ reasonEnd)) :: stack
                  (
                    findMismatchAux(asType(key1), kT2, stackWithReason, seen),
                    findMismatchAux(prop1.tp, vT2, stackWithReason, seen),
                  ) match {
                    case (details, _) if details.nonEmpty => boundary.break(details)
                    case (_, details) if details.nonEmpty => boundary.break(details)
                    case _                                => ()
                  }
                }
            }
          }
          // A new key in the second map is not compatible with the default association of the first map
          val onlyProps2 = props2.removedAll(props1.keySet).toList
          val onlyCompatProps2 = onlyProps2.filter { case (key2, _) => subtype.subType(asType(key2), kT1) }
          for ((key2, prop2) <- onlyCompatProps2) {
            val reasonPostfix =
              s"key ${key2} is not present in the former map but is incompatible with its default association"
            val stackWithReason = Detail(t1, t2, None, Some(reasonPostfix)) :: stack0
            (recur(asType(key2), kT1), findMismatchAux(vT1, prop2.tp, stackWithReason, seen)) match {
              case (l, details) if (l.isEmpty && details.nonEmpty) => boundary.break(details)
              case (_, _)                                          => ()
            }
          }
          // Default associations don't match
          if (kT2 == NoneType && vT2 == NoneType && (recur(kT1, kT2).nonEmpty || recur(vT1, vT2).nonEmpty)) {
            val reasonPostfix = "the latter map has no default association while the first map has one"
            return Detail(t1, t2, None, Some(reasonPostfix)) :: stack
          }
          val reasonPostfix = "the default associations are not compatible"
          val stackWithReason = Detail(t1, t2, None, Some(reasonPostfix)) :: stack0
          val domain2 = subtype.join(kT2, onlyCompatProps2.map(kp => asType(kp._1)))
          (
            findMismatchAux(kT1, domain2, stackWithReason, seen),
            findMismatchAux(vT1, vT2, stackWithReason, seen),
          ) match {
            case (details, _) if details.nonEmpty => details
            case (_, details)                     => details
          }
        }
      case _ =>
        stack
    }
  }

  private def findCandidateInTys(ty: Type, tys: List[Type]): Either[Type, String] = {
    ty match {
      case TupleType(argTys) =>
        val arity = argTys.length
        val filteredByArity = tys.filter {
          case TupleType(argTys2) => argTys2.length == arity
          case AnyTupleType | _: RemoteType | _: UnionType | _: RecordType | _: RefinedRecordType => true
          case _                                                                                  => false
        }
        if (filteredByArity.isEmpty)
          return Right(s"expected union does not contain any tuple type of size ${arity}")
        val filteredByHeadType = filteredByArity.find {
          case TupleType(argTys2) => subtype.subType(argTys.head, argTys2.head)
          case _                  => false
        }
        Left(filteredByHeadType.getOrElse(filteredByArity.head))
      case _ =>
        Left(tys.head)
    }
  }
}
