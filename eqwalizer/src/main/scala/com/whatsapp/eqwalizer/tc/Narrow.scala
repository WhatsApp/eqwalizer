/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Forms.RecDeclTyped
import com.whatsapp.eqwalizer.ast.{RemoteId, TypeVars}
import com.whatsapp.eqwalizer.ast.Types._

class Narrow(pipelineContext: PipelineContext) {
  private val subtype = pipelineContext.subtype
  private val util = pipelineContext.util

  def meet(t1: Type, t2: Type): Type =
    meetAux(t1, t2, Set.empty)(promoteNone = true)

  private def meetAux(t1: Type, t2: Type, seen: Set[RemoteId])(implicit
      promoteNone: Boolean
  ): Type =
    if (subtype.gradualSubType(t1, t2)) t1
    else if (subtype.gradualSubType(t2, t1)) t2
    else
      (t1, t2) match {
        case (RemoteType(rid, args), _) =>
          val body = util.getTypeDeclBody(rid, args)
          meetAux(body, t2, seen)
        case (_, RemoteType(rid, args)) =>
          if (seen(rid) || args.nonEmpty)
            t1
          else {
            val body = util.getTypeDeclBody(rid, args)
            meetAux(t1, body, seen + rid)
          }
        case (DynamicType, _) => DynamicType
        case (_, DynamicType) => DynamicType
        // Composed "refinable" types - refining component by component
        case (UnionType(ty1s), _) => subtype.join(ty1s.map(meetAux(_, t2, seen)))
        case (_, UnionType(ty2s)) =>
          subtype.join(ty2s.map(meetAux(t1, _, seen)(promoteNone)))

        case (TupleType(elems1), TupleType(elems2)) if elems1.size == elems2.size =>
          val elems = elems1.lazyZip(elems2).map(meetAux(_, _, seen))
          if (promoteNone && elems.exists(subtype.isNoneType)) NoneType
          else TupleType(elems)
        case (ListType(et1), ListType(et2)) =>
          val et = meetAux(et1, et2, seen)
          if (promoteNone && subtype.isNoneType(et)) NilType
          else ListType(et)
        case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
          TypeVars.conformForalls(ft1, ft2) match {
            case None => NoneType
            case Some((FunType(forall, args1, res1), FunType(_, args2, res2))) =>
              FunType(
                forall,
                args1.lazyZip(args2).map(subtype.join),
                meetAux(res1, res2, seen)(promoteNone = false),
              )
          }
        case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
          AnyArityFunType(meetAux(resTy1, resTy2, seen))
        case (DictMap(kT1, vT1), DictMap(kT2, vT2)) =>
          DictMap(meetAux(kT1, kT2, seen), meetAux(vT1, vT2, seen))
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
          val reqProps = allReqKeys.toList.sorted.map(k => ReqProp(k, meetAux(kvs1(k), kvs2(k), seen)))
          val optProps = allOptKeys.toList.sorted.map(k => OptProp(k, meetAux(kvs1(k), kvs2(k), seen)))
          if (promoteNone && reqProps.exists(p => subtype.isNoneType(p.tp))) NoneType
          else ShapeMap(reqProps ++ optProps)
        case (rt: RefinedRecordType, t: RecordType) if t == rt.recType => rt
        case (t: RecordType, rt: RefinedRecordType) if t == rt.recType => rt
        case (rt1: RefinedRecordType, rt2: RefinedRecordType) if rt1.recType == rt2.recType =>
          val fields = rt1.fields.keySet ++ rt2.fields.keySet
          val fieldsMeet = fields.map { fieldName =>
            val t1 = rt1.fields.getOrElse(fieldName, AnyType)
            val t2 = rt2.fields.getOrElse(fieldName, AnyType)
            fieldName -> meet(t1, t2)
          }.toMap
          if (promoteNone && fieldsMeet.values.exists(subtype.isNoneType)) NoneType
          else RefinedRecordType(rt1.recType, fieldsMeet)

        // "Non-refinable" types. - Using the main type
        case (DictMap(_, _), ShapeMap(_))           => t1
        case (ShapeMap(_), DictMap(_, _))           => t1
        case (VarType(_), _)                        => t1
        case (_, VarType(_))                        => t1
        case (AnyFunType, FunType(_, _, _))         => t1
        case (FunType(_, _, _), AnyFunType)         => t1
        case (AnyArityFunType(_), FunType(_, _, _)) => t1
        case (FunType(_, _, _), AnyArityFunType(_)) => t1
        case (AnyArityFunType(_), AnyFunType)       => t1
        case (AnyFunType, AnyArityFunType(_))       => t1
        case (OpaqueType(_, _), _)                  => t1
        case (_, OpaqueType(_, _))                  => t1
        // At this point we know for sure that t1 /\ t2 = 0
        case (_, _) =>
          NoneType
      }

  def asListType(t: Type): Option[ListType] =
    extractListElem(t) match {
      case Nil => None
      case ts  => Some(ListType(subtype.join(ts)))
    }

  def asMapType(t: Type): Type =
    t match {
      case DynamicType =>
        DictMap(DynamicType, DynamicType)
      case AnyType | VarType(_) =>
        DictMap(AnyType, AnyType)
      case dictMap: DictMap =>
        dictMap
      case shapeMap: ShapeMap =>
        shapeMap
      case UnionType(ts) =>
        subtype.join(ts.map(asMapType))
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asMapType(body)
      case OpaqueType(rid, _) =>
        DictMap(AnyType, AnyType)
      case _ => NoneType
    }

  def getKeyType(t: Type): Type =
    t match {
      case DictMap(kt, _) => kt
      case ShapeMap(Nil)  => NoneType
      case ShapeMap(_)    => AtomType
      case UnionType(ts)  => subtype.join(ts.map(getKeyType))
      case NoneType       => NoneType
      case _              => throw new IllegalStateException()
    }

  def getValType(t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => subtype.join(props.map(_.tp))
      case UnionType(ts)   => subtype.join(ts.map(getValType))
      case NoneType        => NoneType
      case _               => throw new IllegalStateException()
    }

  def getValType(key: String, t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => props.find(_.key == key).map(_.tp).getOrElse(NoneType)
      case UnionType(ts)   => subtype.join(ts.map(getValType(key, _)))
      case NoneType        => NoneType
      case _               => throw new IllegalStateException()
    }

  def withRequiredProp(k: String, t: Type): Type =
    t match {
      case DynamicType | DictMap(_, _) =>
        t
      case ShapeMap(props) =>
        ShapeMap(props.map {
          case OptProp(`k`, v) =>
            ReqProp(k, v)
          case prop =>
            prop
        })
      case UnionType(ts) =>
        subtype.join(ts.map(withRequiredProp(k, _)))
      case _ =>
        NoneType
    }

  private def extractListElem(t: Type): List[Type] =
    t match {
      case DynamicType =>
        List(DynamicType)
      case AnyType =>
        List(AnyType)
      case UnionType(tys) =>
        tys.toList.flatMap(extractListElem)
      case NilType =>
        List(NoneType)
      case ListType(elemType) =>
        List(elemType)
      case NoneType =>
        List(NoneType)
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        extractListElem(body)
      case VarType(_) =>
        List(AnyType)
      case OpaqueType(rid, _) =>
        List(AnyType)
      case _ =>
        List()
    }

  def asFunType(ty: Type, arity: Int): Option[List[FunType]] =
    asFunTypeAux(ty, arity)

  def asFunTypeAux(ty: Type, arity: Int): Option[List[FunType]] = ty match {
    case DynamicType =>
      Some(List(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType)))
    case AnyFunType if pipelineContext.gradualTyping =>
      Some(List(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType)))
    case AnyArityFunType(resTy) if pipelineContext.gradualTyping =>
      Some(List(FunType(List.empty, List.fill(arity)(DynamicType), resTy)))
    case AnyArityFunType(resTy) =>
      Some(List(FunType(List.empty, List.fill(arity)(NoneType), resTy)))
    case _ if subtype.isNoneType(ty) =>
      Some(List())
    case ft: FunType =>
      if (ft.argTys.size == arity) Some(List(ft))
      else None
    case UnionType(tys) =>
      val subResults = tys.map(asFunTypeAux(_, arity))
      if (subResults.exists(_.isEmpty)) None
      else Some(subResults.toList.flatMap(_.get))
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      asFunTypeAux(body, arity)
    case OpaqueType(rid, _) =>
      None
    case _ =>
      None
  }

  def extractFunTypes(ty: Type, arity: Int): Set[FunType] = ty match {
    case DynamicType =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType))
    case AnyFunType if pipelineContext.gradualTyping =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType))
    case AnyFunType =>
      Set(FunType(List.empty, List.fill(arity)(NoneType), AnyType))
    case ft: FunType =>
      if (ft.argTys.size == arity) Set(ft)
      else Set()
    case AnyArityFunType(resTy) if pipelineContext.gradualTyping =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), resTy))
    case AnyArityFunType(resTy) =>
      Set(FunType(List.empty, List.fill(arity)(NoneType), resTy))
    case UnionType(tys) =>
      tys.flatMap(extractFunTypes(_, arity))
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      extractFunTypes(body, arity)
    case _ =>
      Set()
  }

  def asTupleType(t: Type, arity: Int): List[TupleType] =
    asTupleTypeAux(t, arity)

  def asTupleTypeAux(t: Type, arity: Int): List[TupleType] =
    t match {
      case OpaqueType(rid, _params) =>
        List(TupleType(List.fill(arity)(AnyType)))
      case DynamicType =>
        List(TupleType(List.fill(arity)(DynamicType)))
      case AnyType | VarType(_) =>
        List(TupleType(List.fill(arity)(AnyType)))
      case r: RecordType if arity > 0 =>
        val rec = util.getRecord(r.module, r.name)
        val recFieldTypes = rec match {
          case Some(recDecl) =>
            recDecl.fields.values.toList.map(_.tp)
          case None =>
            if (pipelineContext.gradualTyping)
              List.fill(arity - 1)(DynamicType)
            else
              List.fill(arity - 1)(AnyType)
        }
        val recArity = recFieldTypes.size + 1
        if (arity == recArity) {
          List(TupleType(AtomLitType(r.name) :: recFieldTypes))
        } else
          List()
      case r: RefinedRecordType if arity > 0 =>
        val rec = util.getRecord(r.recType.module, r.recType.name)
        val recFieldTypes = rec match {
          case Some(recDecl) =>
            recDecl.fields.map(f => r.fields.getOrElse(f._1, f._2.tp)).toList
          case None =>
            if (pipelineContext.gradualTyping)
              List.fill(arity - 1)(DynamicType)
            else
              List.fill(arity - 1)(AnyType)
        }
        val recArity = recFieldTypes.size + 1
        if (arity == recArity) {
          List(TupleType(AtomLitType(r.recType.name) :: recFieldTypes))
        } else
          List()
      case AnyTupleType =>
        if (pipelineContext.gradualTyping)
          List(TupleType(List.fill(arity)(DynamicType)))
        else
          List(TupleType(List.fill(arity)(AnyType)))
      case tt: TupleType if tt.argTys.size == arity => List(tt)
      case UnionType(tys)                           => tys.flatMap(asTupleTypeAux(_, arity)).toList
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asTupleTypeAux(body, arity)
      case _ => List()
    }

  private def adjustShapeMap(t: ShapeMap, keyT: Type, valT: Type): Type =
    keyT match {
      case AtomLitType(key) =>
        val oldProps = t.props.filterNot(_.key == key)
        val newProps = ReqProp(key, valT) :: oldProps
        ShapeMap(newProps)
      case _ =>
        if (t.props.isEmpty)
          DictMap(keyT, valT)
        else
          DictMap(subtype.join(AtomType, keyT), t.props.map(_.tp).fold(valT)(subtype.join))
    }

  private def adjustDictMap(dictMap: DictMap, keyT: Type, valT: Type): Type =
    DictMap(subtype.join(dictMap.kType, keyT), subtype.join(dictMap.vType, valT))

  def adjustMapType(mapT: Type, keyT: Type, valT: Type): Type =
    mapT match {
      case DynamicType =>
        DynamicType
      case shapeMap: ShapeMap =>
        adjustShapeMap(shapeMap, keyT, valT)
      case dictMap: DictMap =>
        adjustDictMap(dictMap, keyT, valT)
      case UnionType(elems) =>
        subtype.join(elems.map(adjustMapType(_, keyT, valT)))
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        adjustMapType(body, keyT, valT)
      case NoneType =>
        NoneType
      case _ =>
        throw new IllegalStateException()
    }

  def setAllFieldsOptional(mapT: Type, newValTy: Option[Type] = None): Type =
    mapT match {
      case DynamicType =>
        DynamicType
      case shapeMap: ShapeMap =>
        ShapeMap(shapeMap.props.map {
          case ReqProp(key, tp) => OptProp(key, newValTy.getOrElse(tp))
          case OptProp(key, tp) => OptProp(key, newValTy.getOrElse(tp))
        })
      case dictMap: DictMap =>
        DictMap(dictMap.kType, newValTy.getOrElse(dictMap.vType))
      case UnionType(elems) =>
        val allMaps = elems.map(setAllFieldsOptional(_, newValTy))
        subtype.join(allMaps)
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        setAllFieldsOptional(body, newValTy)
      case NoneType =>
        NoneType
      case _ =>
        throw new IllegalStateException()
    }

  def getRecordField(recDecl: RecDeclTyped, recTy: Type, fieldName: String): Type = {
    val field = recDecl.fields(fieldName)
    recTy match {
      case RefinedRecordType(recType, fields) if recDecl.name == recType.name =>
        fields.getOrElse(fieldName, field.tp)
      case RecordType(name) if recDecl.name == name =>
        field.tp
      case TupleType(argTys) if argTys.size - 1 == recDecl.fields.size && argTys.head == AtomLitType(recDecl.name) =>
        recDecl.fields.zipWithIndex
          .collectFirst { case ((n, _), i) if n == fieldName => i + 1 }
          .map(argTys(_))
          .getOrElse(field.tp)
      case AnyTupleType | DynamicType | VarType(_) | OpaqueType(_, _) =>
        field.tp
      case RemoteType(id, argTys) =>
        getRecordField(recDecl, util.getTypeDeclBody(id, argTys), fieldName)
      case UnionType(argTys) =>
        val fieldTys = argTys.map(getRecordField(recDecl, _, fieldName))
        UnionType(fieldTys)
      case NoneType =>
        NoneType
      case _ =>
        throw new IllegalStateException()
    }
  }
}
