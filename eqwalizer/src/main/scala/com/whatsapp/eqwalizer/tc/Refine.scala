/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.{RemoteId, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._

class Refine(pipelineContext: PipelineContext) {
  private lazy val subtype = pipelineContext.subtype
  private lazy val util = pipelineContext.util

  /** Tries to approximate meet/intersection of t1 and t2 as precise as possible
    * without explicit intersection types. If a precise approximation cannot be expressed
    * it falls back to `t1`.
    * @param t1 the main type
    * @param t2 the secondary type
    * @return Type t with the following properties:
    *         - `t <: t1`
    *         - For any `t0` if `t0 <: t1` and `t0 <: t2` then
    *           `t0 <: t`
    */
  def approxMeet(t1: Type, t2: Type): Type =
    refineAux(t1, t2, Set.empty)(promoteNone = true)

  private def refineAux(t1: Type, t2: Type, seen: Set[RemoteId])(implicit
      promoteNone: Boolean
  ): Type =
    if (subtype.gradualSubType(t1, t2)) t1
    else if (subtype.gradualSubType(t2, t1)) t2
    else
      (t1, t2) match {
        case (RemoteType(rid, args), _) =>
          val body = util.getTypeDeclBody(rid, args)
          refineAux(body, t2, seen)
        case (_, RemoteType(rid, args)) =>
          if (seen(rid) || args.nonEmpty)
            t1
          else {
            val body = util.getTypeDeclBody(rid, args)
            refineAux(t1, body, seen + rid)
          }
        case (DynamicType, _) => DynamicType
        case (_, DynamicType) => DynamicType
        // Composed "refinable" types - refining component by component
        case (UnionType(ty1s), _) => subtype.join(ty1s.map(refineAux(_, t2, seen)))
        case (_, UnionType(ty2s)) =>
          subtype.join(ty2s.map(refineAux(t1, _, seen)(promoteNone)))

        case (TupleType(elems1), TupleType(elems2)) if elems1.size == elems2.size =>
          val elems = elems1.lazyZip(elems2).map(refineAux(_, _, seen))
          if (promoteNone && elems.exists(subtype.isNoneType)) NoneType
          else TupleType(elems)
        case (ListType(et1), ListType(et2)) =>
          val et = refineAux(et1, et2, seen)
          if (promoteNone && subtype.isNoneType(et)) NilType
          else ListType(et)
        case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
          TypeVars.conformForalls(ft1, ft2) match {
            case None => NoneType
            case Some((FunType(forall, args1, res1), FunType(_, args2, res2))) =>
              FunType(
                forall,
                args1.lazyZip(args2).map(subtype.join),
                refineAux(res1, res2, seen)(promoteNone = false),
              )
          }
        case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
          DictMap(refineAux(kT1, kT2, seen), refineAux(vT1, vT2, seen))
        case (ShapeMap(props1), ShapeMap(props2)) =>
          val keys1 = props1.map(_.key).toSet
          val keys2 = props2.map(_.key).toSet
          if (keys1 != keys2) return NoneType
          val reqKeys1 = props1.collect { case ReqProp(k, _) => k }.toSet
          val reqKeys2 = props2.collect { case ReqProp(k, _) => k }.toSet
          val allReqKeys = reqKeys1.union(reqKeys2)
          val allOptKeys = keys1.diff(allReqKeys)
          val kvs1 = props1.map(prop => prop.key -> prop.tp).toMap
          val kvs2 = props2.map(prop => prop.key -> prop.tp).toMap
          val reqProps = allReqKeys.toList.sorted.map(k => ReqProp(k, refineAux(kvs1(k), kvs2(k), seen)))
          val optProps = allOptKeys.toList.sorted.map(k => OptProp(k, refineAux(kvs1(k), kvs2(k), seen)))
          if (promoteNone && reqProps.exists(p => subtype.isNoneType(p.tp))) NoneType
          else ShapeMap(reqProps ++ optProps)
        case (rt: RefinedRecordType, t: RecordType) if t == rt.recType => rt
        case (t: RecordType, rt: RefinedRecordType) if t == rt.recType => rt
        case (rt1: RefinedRecordType, rt2: RefinedRecordType) if rt1.recType == rt2.recType =>
          val fields = rt1.fields.keySet ++ rt2.fields.keySet
          val fieldsMeet = fields.map { fieldName =>
            val t1 = rt1.fields.getOrElse(fieldName, AnyType)
            val t2 = rt2.fields.getOrElse(fieldName, AnyType)
            fieldName -> approxMeet(t1, t2)
          }.toMap
          if (promoteNone && fieldsMeet.values.exists(subtype.isNoneType)) NoneType
          else RefinedRecordType(rt1.recType, fieldsMeet)

        // "Non-refinable" types. - Using the main type
        case (DictMap(_, _), ShapeMap(_))   => t1
        case (ShapeMap(_), DictMap(_, _))   => t1
        case (VarType(_), _)                => t1
        case (_, VarType(_))                => t1
        case (AnyFunType, FunType(_, _, _)) => t1
        case (FunType(_, _, _), AnyFunType) => t1
        case (OpaqueType(_, _), _)          => t1
        case (_, OpaqueType(_, _))          => t1
        // At this point we know for sure that t1 /\ t2 = 0
        case (_, _) =>
          NoneType
      }
}
