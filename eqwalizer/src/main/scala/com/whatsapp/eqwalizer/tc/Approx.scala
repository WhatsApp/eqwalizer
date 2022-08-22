/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Forms.RecDeclTyped
import com.whatsapp.eqwalizer.ast.Types._

// These operations are sound approximations...
// They should be used really carefully, - they can be sound in one context,
// but unsound in another context
class Approx(pipelineContext: PipelineContext) {
  private val subtype = pipelineContext.subtype
  private val util = pipelineContext.util
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
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  def getValType(t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => subtype.join(props.map(_.tp))
      case UnionType(ts)   => subtype.join(ts.map(getValType))
      case NoneType        => NoneType
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  def getValType(key: String, t: Type): Type =
    t match {
      case DictMap(_, vt)  => vt
      case ShapeMap(props) => props.find(_.key == key).map(_.tp).getOrElse(NoneType)
      case UnionType(ts)   => subtype.join(ts.map(getValType(key, _)))
      case NoneType        => NoneType
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
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

  /** "Normalizes" the original type into "union" of fun types.
    * @param ty - original type
    * @param arity - needed arity
    * @return
    *   None if the original type can contain an element E for which is_function(E, arity) is false.
    *   Some(funTys) with the property: subtype.equiv(ty, UnionType(funTys)) is true.
    */
  def asFunType(ty: Type, arity: Int): Option[List[FunType]] =
    asFunTypeAux(ty, arity)

  def asFunTypeAux(ty: Type, arity: Int): Option[List[FunType]] = ty match {
    case DynamicType =>
      Some(List(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType)))
    case AnyFunType if pipelineContext.gradualTyping =>
      Some(List(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType)))
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

  /** Returns precise intersection of `t` and `{any(), any(), ...}`
    *
    * @param t type to refine
    * @param arity tuple arity
    * @return ts such that `t /\ {any(), any(), ...} == UnionType(ts)`
    */
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
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  def isShapeWithKey(mapT: Type, key: String): Boolean =
    mapT match {
      case shapeMap: ShapeMap =>
        shapeMap.props.exists {
          case ReqProp(k, _) => k == key
          case OptProp(_, _) => false
        }
      case UnionType(elems) =>
        elems.forall(isShapeWithKey(_, key))
      case NoneType =>
        true
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        isShapeWithKey(body, key)
      case _ =>
        false
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
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }
}
