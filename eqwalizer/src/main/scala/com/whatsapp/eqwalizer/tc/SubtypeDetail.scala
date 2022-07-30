/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{RemoteId, Show, TypeVars}

object SubtypeDetail {
  private val maxDepth = 4
  lazy val nonVerboseRids = builtinTypes.keys.map(RemoteId("erlang", _, 0)).toSet

  private case class Detail(t1: Type, t2: Type, reasonPrefix: Option[String], reasonPostfix: Option[String])

  private def propDiff(props1: List[Prop], props2: List[Prop]): Option[String] = {
    val props1Map = props1.map { prop => prop.key -> prop }.toMap
    val props2Map = props2.map { prop => prop.key -> prop }.toMap
    val allkeys = (props1 ++ props2).map(_.key).distinct.sorted

    val padTo = allkeys.map(_.length).max
    def showProp(prop: Prop): String = prop match {
      case ReqProp(key, tp) => s"${key.padTo(padTo, ' ')} := ..."
      case OptProp(key, tp) => s"${key.padTo(padTo, ' ')} => ..."
    }

    def showMinus(prop: Prop): String = s"\n-    ${showProp(prop)}"
    def showPlus(prop: Prop): String = s"\n+    ${showProp(prop)}"

    var hasMore = false

    val diff = new StringBuilder()
    for (key <- allkeys) (props1Map.get(key), props2Map.get(key)) match {
      case (None, Some(prop2: ReqProp)) =>
        diff ++= showMinus(prop2)
      case (Some(prop1: OptProp), Some(prop2: ReqProp)) =>
        diff ++= showMinus(prop2)
        diff ++= showPlus(prop1)
      case (Some(prop1), None) =>
        diff ++= showPlus(prop1)
      case (_, Some(_)) =>
        hasMore = true
      case (None, None) =>
        // $COVERAGE-OFF$
        throw new IllegalStateException()
      // $COVERAGE-ON$
    }

    if (diff.isEmpty) None
    else {
      if (hasMore) {
        diff ++= s"\n     ..."
      }
      Some(s"These associations do not match:\n\n  #S{${diff.toString}\n  }")
    }
  }
}

class SubtypeDetail(pipelineContext: PipelineContext) {
  import SubtypeDetail._
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype

  def explain(got: Type, expected: Type): Option[String] =
    findSubtypeMismatch(got, expected) match {
      case Nil =>
        // $COVERAGE-OFF$
        None // put debug assert here in T120664791, we expect to be able to produce an error explanation
      // $COVERAGE-ON$
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
              s"$msgBefore  $showT1 is not a subtype of $showT2$msgAfter"
            }
            .mkString("\n  because\n")
        )
    }

  private def findSubtypeMismatch(t1: Type, t2: Type): List[Detail] =
    findMismatchAux(t1, t2, Nil, Set.empty).reverse.take(maxDepth)

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

      case (RemoteType(rid, args), _) =>
        util.getTypeDeclBody(rid, args) match {
          case body @ OpaqueType(rid2, _) if rid2 == rid =>
            findMismatchAux(body, t2, stack0, seen)
          case body =>
            val stack1 = findMismatchAux(body, t2, stack, seen + (t1 -> t2))
            if (nonVerboseRids(rid) && stack1.nonEmpty) stack
            else stack1
        }
      case (_, RemoteType(rid, args)) =>
        util.getTypeDeclBody(rid, args) match {
          case body @ OpaqueType(rid2, _) if rid2 == rid =>
            findMismatchAux(t1, body, stack0, seen)
          case body =>
            val stack1 = findMismatchAux(t1, body, stack, seen + (t1 -> t2))
            if (nonVerboseRids(rid) && stack1.nonEmpty) stack
            else stack1
        }

      case (OpaqueType(id1, tys1), OpaqueType(id2, tys2)) =>
        if (id1 != id2) stack
        else recurSeq(tys1.zip(tys2))

      case (ut: UnionType, _) =>
        val tys1 = util.flattenUnions(ut)
        val stacks = tys1.map(recur(_, t2))
        val noBranchMatches = stacks.forall(_.nonEmpty)
        if (noBranchMatches) stack
        else stacks.find(_.nonEmpty).getOrElse(Nil)
      case (_, ut: UnionType) =>
        val tys2 = util.flattenUnions(ut)
        var firstMismatch: List[Detail] = null
        for (ty2 <- tys2) {
          val mismatch = recur(t1, ty2)
          if (mismatch.isEmpty) return Nil
          else if (firstMismatch == null) firstMismatch = mismatch
        }
        firstMismatch
      case (AtomLitType(_), AtomType) => Nil
      case (TupleType(_), AnyTupleType) =>
        Nil
      case (FunType(_, argTys, _), AnyFunType) =>
        if (recurSeq(argTys.map((AnyType, _))).nonEmpty) stack
        else Nil
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
            recurSeq(refRec.fields.toList.map(f => (recDecl.fields(f._1).tp, f._2)))
          case None =>
            stack
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        util.getRecord(refRec1.recType.module, refRec1.recType.name) match {
          case Some(recDecl) =>
            recurSeq(refRec2.fields.collect {
              case (fName, fTy) if refRec1.fields.contains(fName) => (refRec1.fields(fName), fTy)
              case (fName, fTy)                                   => (recDecl.fields(fName).tp, fTy)
            }.toList)
          case None =>
            stack
        }
      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        for (((itemTy1, indexFrom0), itemTy2) <- tys1.zipWithIndex.lazyZip(tys2)) {
          val index = indexFrom0 + 1
          val reasonPrefix = s"at tuple index $index:"
          val stackWithReason = Detail(t1, t2, Some(reasonPrefix), None) :: stack0
          findMismatchAux(itemTy1, itemTy2, stackWithReason, seen) match {
            case Nil     => ()
            case details => return details
          }
        }
        Nil
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
      case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
        recurSeq((kT1, kT2) :: (vT1, vT2) :: Nil)
      case (ShapeMap(props), DictMap(kT, vT)) =>
        val shapeDomain = subtype.join(props.map(prop => AtomLitType(prop.key)))
        val shapeCodomain = subtype.join(props.map(_.tp))
        recurSeq((shapeDomain, kT) :: (shapeCodomain, vT) :: Nil)
      case (ShapeMap(props1), ShapeMap(props2)) =>
        propDiff(props1, props2) match {
          case reasonPostfix @ Some(_) =>
            Detail(t1, t2, None, reasonPostfix) :: stack0
          case None =>
            val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
            for (prop1 <- props1) {
              val key = prop1.key
              val ty1 = prop1.tp
              val ty2 = kvs2(prop1.key)
              val reasonPrefix = s"at shape key '$key':"
              val stackWithReason = Detail(t1, t2, Some(reasonPrefix), None) :: stack0
              findMismatchAux(ty1, ty2, stackWithReason, seen) match {
                case Nil     => ()
                case details => return details
              }
            }
            Nil
        }
      case (DictMap(kT, vT), ShapeMap(_)) if subtype.isDynamicType(kT) && subtype.isDynamicType(vT) =>
        Nil
      case _ =>
        stack
    }
  }
}
