/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types.Key.asType
import com.whatsapp.eqwalizer.ast.{RemoteId, Show, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._

/**
 * This module operates in two steps: it first finds a mismatch between two types (findMismatch) and then formats it
 * (explainMismatch).
 * A mismatch is essentially a path in the two types, where the final step is a basic type incompatibility.
 * Because the two types do not necessarily have the same form (because of, e.g., unions), findMismatch rebuilds the
 * two types to only keep the incompatibility, building the mismatch path along the way.
 * Additionally, because of variance, the types have to be rebuilt separately from the mismatch path, so that t1 is
 * always the expression's type and t2 the expected type.
 * As such, explainMismatch assumes a correspondence between its arguments that cannot be strictly enforced statically.
 */

object TypeMismatch {
  lazy val nonVerboseRids = builtinTypes.keys.map(RemoteId("erlang", _, 0)).toSet

  /**
   * The score is a rough estimate of how close the two matched types are.
   * A score of 100 means the two given types are static subtypes, in that case mismatch will be None
   * A score of 90 means the two given types are gradual subtypes, mismatch will still be None
   * Below that, the scores are given heuristically, e.g., matching top-level constructors will have a high score
   * while completely incompatible types (different constructors or atoms) will have a score of 0.
   *
   * Scores are used to select union candidates: amongst incompatible pairs, the "worst match" will be selected
   * for left hand side unions, while the "best match" will be selected for right hand side unions.
   */
  private case class Details(mismatch: Option[(Type, Type, Mismatch)], score: Integer)

  sealed trait Mismatch
  // Base cases
  case object Incompatible extends Mismatch
  case object ParametersMismatch extends Mismatch
  case class MissingReqKeys(keys: Set[Key]) extends Mismatch
  case class MissingKey(key: Key) extends Mismatch
  case object MissingDefault extends Mismatch
  // Recursive cases
  case class ArgTyMismatch(argN: Integer, m: Mismatch) extends Mismatch
  case class ResTyMismatch(m: Mismatch) extends Mismatch
  case class RecordFieldMismatch(field: String, m: Mismatch) extends Mismatch
  case class MapKeyMismatch(k: Key, m: Mismatch) extends Mismatch
  case class MapPropMismatch(k: Key, m: Mismatch) extends Mismatch
  case class MapDefaultKeyMismatch(m: Mismatch) extends Mismatch
  case class MapDefaultValMismatch(m: Mismatch) extends Mismatch
}

class TypeMismatch(pipelineContext: PipelineContext) {
  import TypeMismatch._
  private lazy val util = pipelineContext.util
  private lazy val subtype = pipelineContext.subtype
  private lazy val show = new Show(Some(pipelineContext))

  def explain(t1: Type, t2: Type): Option[String] = {
    findMismatch(t1, t2, Set()).mismatch match {
      case Some((newT1, newT2, Incompatible)) if t1 == newT1 && t2 == newT2 =>
        // Do not return a message if it does not add info
        None
      case Some((newT1, newT2, mismatch)) =>
        val explanation = explainMismatch(newT1, newT2, mismatch, 1)
        val intro = "Because in the expression's type:"
        Some(s"$intro\n$explanation")
      case _ =>
        // Should not happen if subtyping failed
        None
    }
  }

  def showLimitChars(tp: Type, chars: Integer = 30): String = {
    val fullShow = show.show(tp)
    if (fullShow.size >= chars)
      show.showTruncated(tp)
    else
      fullShow
  }

  private def explainMismatch(t1: Type, t2: Type, mismatch: Mismatch, depth: Integer): String = {
    val prefix = "  " * depth
    (mismatch, t1, t2) match {
      case (Incompatible, _: RemoteType, _) | (Incompatible, _, _: RemoteType) =>
        val got = s"${prefix}Here the type is:     ${showLimitChars(t1, chars = 60)}"
        val exp = s"${prefix}Context expects type: ${showLimitChars(t2, chars = 60)}"
        List(got, exp).mkString("\n")
      case (Incompatible, UnionType(_), _) =>
        val got = s"${prefix}Here the type is:     ${showLimitChars(t1, chars = 60)}"
        val exp = s"${prefix}Context expects type: ${showLimitChars(t2, chars = 60)}"
        val add = s"${prefix}No candidate of the expression's type matches the expected type."
        List(got, exp, add).mkString("\n")
      case (Incompatible, _, UnionType(_)) =>
        val got = s"${prefix}Here the type is:     ${showLimitChars(t1, chars = 60)}"
        val exp = s"${prefix}Context expects type: ${showLimitChars(t2, chars = 60)}"
        val add = s"${prefix}No candidate matches in the expected union."
        List(got, exp, add).mkString("\n")
      case (Incompatible, _, _) =>
        val got = s"${prefix}Here the type is:     ${showLimitChars(t1, chars = 60)}"
        val exp = s"${prefix}Context expects type: ${showLimitChars(t2, chars = 60)}"
        List(got, exp).mkString("\n")
      case (ParametersMismatch, ft1: FunType, ft2: FunType) =>
        val cnt1 = ft1.forall.size
        val cnt2 = ft2.forall.size
        def plural(n: Int): String = if (n != 1) "s" else ""
        val got =
          s"${prefix}Here the type is:     ${showLimitChars(ft1, chars = 40)} with $cnt1 type parameter${plural(cnt1)}"
        val exp =
          s"${prefix}Context expects type: ${showLimitChars(ft2, chars = 40)} with $cnt2 type parameter${plural(cnt2)}"
        val add = s"${prefix}The number of type parameters doesn't match."
        List(got, exp, add).mkString("\n")
      case (MissingReqKeys(keys), MapType(props, _, _), _) =>
        val keyList = keys.toList
        val kvReqList = keyList.map(k => s"$k := ...").appended("...").mkString(", ")
        // Quadratic, but important to preserve key ordering for better display
        val kvOptList = keyList.filter(props.contains).map(k => s"$k => ...").appended("...").mkString(", ")
        val got = s"${prefix}Here the type is:     #{$kvOptList}"
        val exp = s"${prefix}Context expects type: #{$kvReqList}"
        val add =
          s"${prefix}The type of the expression is missing the following required keys: ${keys.toList.map(_.toString).mkString(", ")}."
        List(got, exp, add).mkString("\n")
      case (MissingKey(key), MapType(props, _, _), _) =>
        val kv =
          if (props(key).req) s"${key} := ..."
          else s"${key} => ..."
        val got = s"${prefix}Here the type is:     #{$kv}"
        val exp = s"${prefix}Context expects type: #{...}"
        val add = s"${prefix}The expected map has no corresponding key for: $key."
        List(got, exp, add).mkString("\n")
      case (MissingDefault, MapType(_, kType, vType), _) =>
        val got = s"${prefix}Here the type is:     #{${showLimitChars(kType)} => ${showLimitChars(vType)}}"
        val exp = s"${prefix}Context expects type: #{...} (no default association)"
        val add = s"${prefix}The expected map has no default association while the type of the expression has one."
        List(got, exp, add).mkString("\n")
      case (ArgTyMismatch(argN, argMismatch), FunType(_, argTys1, resTy), FunType(_, argTys2, _)) =>
        val argExplain = explainMismatch(argTys1(argN), argTys2(argN), argMismatch, depth + 1)
        val argsFmt = argTys1.map(showLimitChars(_)).updated(argN, s"\n$argExplain\n$prefix")
        argsFmt.mkString(s"${prefix}fun((", ", ", s") -> ${showLimitChars(resTy)})")
      case (ArgTyMismatch(argN, argMismatch), TupleType(argTys1), TupleType(argTys2)) =>
        val argExplain = explainMismatch(argTys1(argN), argTys2(argN), argMismatch, depth + 1)
        val argsFmt = argTys1.map(showLimitChars(_)).updated(argN, s"\n$argExplain\n$prefix")
        argsFmt.mkString(s"$prefix{ ", ", ", "}")
      case (ArgTyMismatch(_, argMismatch), ListType(argTy1), ListType(argTy2)) =>
        val argExplain = explainMismatch(argTy1, argTy2, argMismatch, depth + 1)
        s"$prefix[\n$argExplain\n$prefix]"
      case (ArgTyMismatch(argN, argMismatch), OpaqueType(rid, argTys1), OpaqueType(_, argTys2)) =>
        val argExplain = explainMismatch(argTys1(argN), argTys2(argN), argMismatch, depth + 1)
        val argsFmt = argTys1.map(showLimitChars(_)).updated(argN, s"\n$argExplain\n$prefix")
        argsFmt.mkString(s"${prefix}Opaque $rid(", ", ", ")")
      case (ResTyMismatch(resMismatch), FunType(_, argTys, resTy1), FunType(_, _, resTy2)) =>
        val resExplain = explainMismatch(resTy1, resTy2, resMismatch, depth + 1)
        val argsFmt = argTys.map(showLimitChars(_)).mkString("fun((", ", ", ") ->")
        s"$prefix$argsFmt\n$resExplain\n$prefix)"
      case (ResTyMismatch(resMismatch), AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
        val resExplain = explainMismatch(resTy1, resTy2, resMismatch, depth + 1)
        s"${prefix}fun((...) ->\n$resExplain\n$prefix)"
      case (
            RecordFieldMismatch(fieldName, fieldMismatch),
            RefinedRecordType(RecordType(rec), fields1),
            RefinedRecordType(_, fields2),
          ) =>
        val fieldExplain = explainMismatch(fields1(fieldName), fields2(fieldName), fieldMismatch, depth + 1)
        s"$prefix#$rec{$fieldName ::\n$fieldExplain\n$prefix}"
      case (MapKeyMismatch(key, keyMismatch), MapType(_, _, _), MapType(_, kT2, _)) =>
        val keyExplain = explainMismatch(Key.asType(key), kT2, keyMismatch, depth + 1)
        s"$prefix#{ incompatible map key $key:\n$keyExplain\n$prefix, ... }"
      case (MapPropMismatch(key, propMismatch), MapType(props1, kT1, vT1), MapType(props2, _, vT2)) =>
        if (!props1.contains(key)) {
          val propExplain = explainMismatch(vT1, props2(key).tp, propMismatch, depth + 1)
          val assoc =
            if (props2(key).req) ":="
            else "=>"
          val add =
            s"${prefix}The context introduces a new association $key $assoc ${showLimitChars(props2(key).tp, 20)} which is incompatible with the expression's default association."
          s"$prefix#{ ${show.show(kT1)} =>\n$propExplain\n$prefix, ... }\n$add"
        } else if (!props2.contains(key)) {
          val propExplain = explainMismatch(props1(key).tp, vT2, propMismatch, depth + 1)
          s"$prefix#{ $key =>\n$propExplain\n$prefix, ... }"
        } else {
          val propExplain = explainMismatch(props1(key).tp, props2(key).tp, propMismatch, depth + 1)
          s"$prefix#{ $key =>\n$propExplain\n$prefix, ... }"
        }
      case (MapDefaultKeyMismatch(keyMismatch), MapType(_, kT1, _), MapType(_, kT2, _)) =>
        val keyExplain = explainMismatch(kT1, kT2, keyMismatch, depth + 1)
        s"$prefix#{ map domains are incompatible:\n$keyExplain\n$prefix, ... }"
      case (MapDefaultValMismatch(valMismatch), MapType(_, kT1, vT1), MapType(_, _, vT2)) =>
        val valExplain = explainMismatch(vT1, vT2, valMismatch, depth + 1)
        s"$prefix#{ ${show.show(kT1)} =>\n$valExplain\n$prefix, ... }"
      case _ =>
        // Should not happen, but return a basic incompatibility message
        val got = s"${prefix}Expression has type:   ${show.show(t1)}"
        val exp = s"${prefix}Context expected type: ${show.show(t2)}"
        List(got, exp).mkString("\n")
    }
  }

  private def findMismatch(
      t1: Type,
      t2: Type,
      seen: Set[(Type, Type)],
  ): Details = {

    def findMismatchWithIndex[T](indexedPairs: List[((Type, Type), T)]): Option[(Type, Type, Mismatch, T)] = {
      indexedPairs.iterator
        .map { case ((t1, t2), idx) => (idx, findMismatch(t1, t2, seen).mismatch) }
        .collectFirst { case (index, Some((t1, t2, mismatch))) =>
          (t1, t2, mismatch, index)
        }
    }

    (t1, t2) match {
      case (_, _) if seen((t1, t2)) =>
        Details(None, 100)
      case (_, _) if t1 == t2 =>
        Details(None, 100)

      case (_, AnyType) =>
        Details(None, 100)
      case (NoneType, _) =>
        Details(None, 100)

      case (DynamicType, _) =>
        Details(None, 90)
      case (_, DynamicType) =>
        Details(None, 90)

      case (BoundedDynamicType(_), _) =>
        Details(None, 90)
      case (_, BoundedDynamicType(bound)) =>
        findMismatch(t1, bound, seen)

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        val details = findMismatch(body, t2, seen + (t1 -> t2))
        if (nonVerboseRids(rid) && details.mismatch.nonEmpty) Details(Some(t1, t2, Incompatible), details.score)
        else details
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        val details = findMismatch(t1, body, seen + (t1 -> t2))
        if (nonVerboseRids(rid) && details.mismatch.nonEmpty) Details(Some(t1, t2, Incompatible), details.score)
        else details

      case (OpaqueType(id1, tys1), OpaqueType(id2, tys2)) =>
        if (id1 != id2) Details(Some(t1, t2, Incompatible), 0)
        else {
          findMismatchWithIndex(tys1.zip(tys2).zipWithIndex) match {
            case None => Details(None, 100)
            case Some((argTy1, argTy2, mismatch, index)) =>
              val argTys1 = tys1.updated(index, argTy1)
              val argTys2 = tys2.updated(index, argTy2)
              Details(Some(OpaqueType(id1, argTys1), OpaqueType(id2, argTys2), ArgTyMismatch(index, mismatch)), 80)
          }
        }

      case (ut: UnionType, _) =>
        val tys1 = util.flattenUnions(ut)
        val details = tys1.map(findMismatch(_, t2, seen))
        val lowest = details.minBy(_.score)
        if (details.forall(_.mismatch.nonEmpty))
          Details(Some(subtype.join(tys1), t2, Incompatible), lowest.score)
        else
          lowest
      case (_, ut: UnionType) =>
        val tys2 = util.flattenUnions(ut)
        val details = tys2.map(findMismatch(t1, _, seen))
        val highest = details.maxBy(_.score)
        if (highest.score < 50)
          Details(Some(t1, subtype.join(tys2), Incompatible), highest.score)
        else
          highest

      case (AtomLitType(_), AtomType) =>
        Details(None, 100)
      case (TupleType(_), AnyTupleType) =>
        Details(None, 100)
      case (RecordType(_), AnyTupleType) =>
        Details(None, 100)
      case (RefinedRecordType(_, _), AnyTupleType) =>
        Details(None, 100)

      case (r: RecordType, t: TupleType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            findMismatch(recordAsTuple(recDecl), t, seen)
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }
      case (t: TupleType, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            findMismatch(t, recordAsTuple(recDecl), seen)
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }
      case (r: RefinedRecordType, t: TupleType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            findMismatch(refinedRecordAsTuple(recDecl, r), t, seen)
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }
      case (t: TupleType, r: RefinedRecordType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            findMismatch(t, refinedRecordAsTuple(recDecl, r), seen)
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }
      case (refRec: RefinedRecordType, rec: RecordType) if refRec.recType.name == rec.name =>
        Details(None, 100)
      case (rec: RecordType, refRec: RefinedRecordType) if refRec.recType.name == rec.name =>
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            val indexedPairs = refRec.fields.map { case (fieldName, fieldTy) =>
              ((recDecl.fields(fieldName).tp, fieldTy), fieldName)
            }.toList
            findMismatchWithIndex(indexedPairs) match {
              case None => Details(None, 100)
              case Some((fieldTy1, fieldTy2, mismatch, fieldName)) =>
                val newTy1 = RefinedRecordType(rec, Map(fieldName -> fieldTy1))
                val newTy2 = RefinedRecordType(rec, Map(fieldName -> fieldTy2))
                Details(Some(newTy1, newTy2, RecordFieldMismatch(fieldName, mismatch)), 80)
            }
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        val rec = refRec1.recType
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            val indexedPairs = refRec2.fields.map { case (fieldName, fieldTy2) =>
              val fieldTy1 = refRec1.fields.getOrElse(fieldName, recDecl.fields(fieldName).tp)
              ((fieldTy1, fieldTy2), fieldName)
            }.toList
            findMismatchWithIndex(indexedPairs) match {
              case None => Details(None, 100)
              case Some((fieldTy1, fieldTy2, mismatch, fieldName)) =>
                val newTy1 = RefinedRecordType(rec, Map(fieldName -> fieldTy1))
                val newTy2 = RefinedRecordType(rec, Map(fieldName -> fieldTy2))
                Details(Some(newTy1, newTy2, RecordFieldMismatch(fieldName, mismatch)), 80)
            }
          case None =>
            Details(Some(t1, t2, Incompatible), 20)
        }

      case (AnyTupleType, TupleType(_)) =>
        Details(None, 90)
      case (AnyTupleType, RecordType(_)) =>
        Details(None, 90)
      case (AnyTupleType, RefinedRecordType(_, _)) =>
        Details(None, 90)

      case (FunType(_, _, _), AnyFunType) =>
        Details(None, 100)
      case (AnyFunType, FunType(_, _, _)) =>
        Details(None, 90)
      case (AnyArityFunType(_), AnyFunType) =>
        Details(None, 100)
      case (AnyFunType, AnyArityFunType(_)) =>
        Details(None, 90)

      case (FunType(_, argTys, resTy1), AnyArityFunType(resTy2)) =>
        val resMismatch = findMismatch(resTy1, resTy2, seen)
        resMismatch.mismatch match {
          case None => resMismatch
          case Some((newResTy1, newResTy2, mismatch)) =>
            val newTy1 = FunType(List(), argTys, newResTy1)
            val newTy2 = FunType(List(), List.fill(argTys.length)(AnyType), newResTy2)
            Details(Some(newTy1, newTy2, ResTyMismatch(mismatch)), 60)
        }
      case (AnyArityFunType(resTy1), FunType(_, argTys, resTy2)) =>
        val resMismatch = findMismatch(resTy1, resTy2, seen)
        resMismatch.mismatch match {
          case None => resMismatch
          case Some((newResTy1, newResTy2, mismatch)) =>
            val newTy1 = FunType(List(), List.fill(argTys.length)(AnyType), newResTy1)
            val newTy2 = FunType(List(), argTys, newResTy2)
            Details(Some(newTy1, newTy2, ResTyMismatch(mismatch)), 60)
        }
      case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
        val resMismatch = findMismatch(resTy1, resTy2, seen)
        resMismatch.mismatch match {
          case None => resMismatch
          case Some((newResTy1, newResTy2, mismatch)) =>
            val newTy1 = AnyArityFunType(newResTy1)
            val newTy2 = AnyArityFunType(newResTy2)
            Details(Some(newTy1, newTy2, ResTyMismatch(mismatch)), 60)
        }

      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        val indexedPairs = tys1.zip(tys2).zipWithIndex
        findMismatchWithIndex(indexedPairs) match {
          case None => Details(None, 100)
          case Some((argTy1, argTy2, mismatch, index)) =>
            val newTy1 = TupleType(tys1.updated(index, argTy1))
            val newTy2 = TupleType(tys2.updated(index, argTy2))
            // Favor tuples that have a first compatible first component
            val score =
              if (index > 0) 80
              else 60
            Details(Some(newTy1, newTy2, ArgTyMismatch(index, mismatch)), score)
        }

      case (NilType, ListType(_)) =>
        Details(None, 100)
      case (ListType(ty1), NilType) =>
        val details = findMismatch(ty1, NoneType, seen)
        details.mismatch match {
          case None => details
          case Some((argTy1, _, mismatch)) =>
            Details(Some(ListType(argTy1), ListType(NoneType), ArgTyMismatch(0, mismatch)), 40)
        }
      case (ListType(ty1), ListType(ty2)) =>
        val details = findMismatch(ty1, ty2, seen)
        details.mismatch match {
          case None => details
          case Some((argTy1, argTy2, mismatch)) =>
            Details(Some(ListType(argTy1), ListType(argTy2), ArgTyMismatch(0, mismatch)), 80)
        }

      case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
        TypeVars.conformForalls(ft1, ft2) match {
          case None =>
            Details(Some(ft1, ft2, ParametersMismatch), 60)
          case Some((FunType(_, argTys1, resTy1), FunType(_, argTys2, resTy2))) =>
            val resMismatch = findMismatch(resTy1, resTy2, seen)
            val argsMismatch = findMismatchWithIndex(argTys2.zip(argTys1).zipWithIndex)
            (resMismatch.mismatch, argsMismatch) match {
              case (Some((newRes1, newRes2, mismatch)), _) =>
                val newTy1 = FunType(List(), argTys1, newRes1)
                val newTy2 = FunType(List(), argTys2, newRes2)
                Details(Some(newTy1, newTy2, ResTyMismatch(mismatch)), 80)
              case (_, Some((newArg1, newArg2, mismatch, index))) =>
                val newTy1 = FunType(List(), argTys1.updated(index, newArg1), resTy1)
                val newTy2 = FunType(List(), argTys2.updated(index, newArg2), resTy2)
                Details(Some(newTy1, newTy2, ArgTyMismatch(index, mismatch)), 80)
              case (_, _) =>
                Details(None, 100)
            }
        }

      case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
        val tolerantSubtype = subtype.isDynamicType(kT1) && subtype.isDynamicType(vT1)
        // Some heuristic to determine map "closeness"
        val commonKeys = props1.keySet.intersect(props2.keySet)
        val allKeys = props1.keySet.union(props2.keySet)
        val score =
          if (allKeys.size == 0) 80
          else 40 + 40 * commonKeys.size / allKeys.size
        // Required keys don't match
        val reqKeys1 = props1.collect { case (k, Prop(true, _)) => k }.toSet
        val reqKeys2 = props2.collect { case (k, Prop(true, _)) => k }.toSet
        if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) {
          val missingReqKeys = reqKeys2.removedAll(reqKeys1)
          return Details(Some(t1, t2, MissingReqKeys(missingReqKeys)), score)
        }
        // A key in the LHS doesn't match the RHS
        for ((key1, prop1) <- props1) {
          props2.get(key1) match {
            case Some(prop2) =>
              findMismatch(prop1.tp, prop2.tp, seen).mismatch match {
                case Some((prop1Ty, prop2Ty, mismatch)) =>
                  val newTy1 = MapType(props1.updated(key1, Prop(prop1.req, prop1Ty)), kT1, vT1)
                  val newTy2 = MapType(props2.updated(key1, Prop(prop2.req, prop2Ty)), kT2, vT2)
                  return Details(Some(newTy1, newTy2, MapPropMismatch(key1, mismatch)), score)
                case None =>
              }
            case None =>
              // key1 is declared in t1 but not in t2...
              if (kT2 == NoneType) {
                // ... and t2 has no default association
                return Details(Some(t1, t2, MissingKey(key1)), score)
              } else {
                // ... and the default association of t2 is not compatible
                val mismatchKey = findMismatch(asType(key1), kT2, seen).mismatch
                val mismatchVal = findMismatch(prop1.tp, vT2, seen).mismatch
                (mismatchKey, mismatchVal) match {
                  case (Some((_, newKT2, mismatch)), _) =>
                    val newTy2 = MapType(props2, newKT2, vT2)
                    return Details(Some(t1, newTy2, MapKeyMismatch(key1, mismatch)), score)
                  case (_, Some((newProp1Ty, newVT2, mismatch))) =>
                    val newTy1 = MapType(props1.updated(key1, Prop(prop1.req, newProp1Ty)), kT1, vT1)
                    val newTy2 = MapType(props2, kT2, newVT2)
                    return Details(Some(newTy1, newTy2, MapPropMismatch(key1, mismatch)), score)
                  case (_, _) =>
                }
              }
          }
        }
        // Default associations don't match
        val mismatchKey = findMismatch(kT1, kT2, seen).mismatch
        val mismatchVal = findMismatch(vT1, vT2, seen).mismatch
        if (kT2 == NoneType && vT2 == NoneType && (mismatchKey.nonEmpty || mismatchVal.nonEmpty))
          return Details(Some(t1, t2, MissingDefault), score)
        val onlyProps2 = props2.removedAll(props1.keySet).toList
        val onlyCompatProps2 = onlyProps2.filter { case (key2, _) => subtype.subType(asType(key2), kT1) }
        val domain2 = subtype.join(kT2, onlyCompatProps2.map(kp => asType(kp._1)))
        val mismatchDomain = findMismatch(kT1, domain2, seen).mismatch
        (mismatchDomain, mismatchVal) match {
          case (Some((newKT1, newKT2, mismatch)), _) =>
            val newTy1 = MapType(props1, newKT1, vT1)
            val newTy2 = MapType(props2, newKT2, vT2)
            return Details(Some(newTy1, newTy2, MapDefaultKeyMismatch(mismatch)), score)
          case (_, Some((newVT1, newVT2, mismatch))) =>
            val newTy1 = MapType(props1, kT1, newVT1)
            val newTy2 = MapType(props2, kT2, newVT2)
            return Details(Some(newTy1, newTy2, MapDefaultValMismatch(mismatch)), score)
          case (_, _) =>
        }
        // A new key in the second map is not compatible with the default association of the first map
        for ((key2, prop2) <- onlyCompatProps2) {
          val mismatchVal = findMismatch(vT1, prop2.tp, seen).mismatch
          mismatchVal match {
            case Some((newVT1, newProp2Ty, mismatch)) =>
              val newTy1 = MapType(props1, kT1, newVT1)
              val newTy2 = MapType(props2.updated(key2, Prop(prop2.req, newProp2Ty)), kT2, vT2)
              return Details(Some(newTy1, newTy2, MapPropMismatch(key2, mismatch)), score)
            case _ =>
          }
        }
        Details(None, 100)

      case _ =>
        Details(Some(t1, t2, Incompatible), 0)
    }
  }
}
