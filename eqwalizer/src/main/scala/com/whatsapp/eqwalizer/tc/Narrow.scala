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
          if (seen(rid) || args.nonEmpty) t1
          else {
            val body = util.getTypeDeclBody(rid, args)
            meetAux(t1, body, seen + rid)
          }
        case (BoundedDynamicType(b1), DynamicType) =>
          BoundedDynamicType(b1)
        case (DynamicType, BoundedDynamicType(b2)) =>
          BoundedDynamicType(b2)
        case (DynamicType, t) => BoundedDynamicType(t)
        case (t, DynamicType) => BoundedDynamicType(t)
        case (BoundedDynamicType(b1), BoundedDynamicType(b2)) =>
          BoundedDynamicType(meetAux(b1, b2, seen))
        case (BoundedDynamicType(b1), _) =>
          BoundedDynamicType(meetAux(b1, t2, seen))
        case (_, BoundedDynamicType(b2)) =>
          BoundedDynamicType(meetAux(t1, b2, seen))
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
        case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
          var props: Map[Key, Prop] = Map()
          val keys = props1.keySet ++ props2.keySet
          for (key <- keys) {
            val prop1 = props1.get(key)
            val prop2 = props2.get(key)
            val keyT = Key.asType(key)
            if ((prop1.isEmpty && !subtype.subType(keyT, kT1)) || (prop2.isEmpty && !subtype.subType(keyT, kT2))) {
              return NoneType
            }
            val propT1 = prop1.map(_.tp).getOrElse(kT1)
            val propT2 = prop2.map(_.tp).getOrElse(kT2)
            val req = prop1.exists(_.req) || prop2.exists(_.req)
            val meetType = meetAux(propT1, propT2, seen)
            if (promoteNone && req && subtype.isNoneType(meetType)) {
              return NoneType
            }
            props += (key -> Prop(req, meetType))
          }
          MapType(props, meetAux(kT1, kT2, seen), meetAux(vT1, vT2, seen))
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

  private def dynamicMap(t: MapType): MapType = {
    val MapType(props, kType, vType) = t
    MapType(
      props.map { case (key, Prop(req, tp)) =>
        (key, Prop(req, boundedDynamic(tp)))
      },
      boundedDynamic(kType),
      boundedDynamic(vType),
    )
  }

  private def boundedDynamic(t: Type): Type = {
    t match {
      case DynamicType           => DynamicType
      case BoundedDynamicType(b) => BoundedDynamicType(b)
      case NoneType              => NoneType
      case AnyType               => DynamicType
      case _                     => BoundedDynamicType(t)
    }
  }

  def asMapTypes(t: Type): Set[MapType] =
    t match {
      case DynamicType =>
        Set(MapType(Map(), DynamicType, DynamicType))
      case BoundedDynamicType(bound) =>
        asMapTypes(bound).map(dynamicMap)
      case AnyType | VarType(_) =>
        Set(MapType(Map(), AnyType, AnyType))
      case mapType: MapType =>
        Set(mapType)
      case UnionType(ts) =>
        ts.flatMap(asMapTypes)
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asMapTypes(body)
      case OpaqueType(rid, _) =>
        Set(MapType(Map(), AnyType, AnyType))
      case _ => Set()
    }

  def getKVType(t: MapType): Set[TupleType] = {
    t.props.map { case (key, prop) =>
      TupleType(List(Key.asType(key), prop.tp))
    }.toSet + TupleType(List(t.kType, t.vType))
  }

  def getKeyType(t: MapType)(implicit reqOnly: Boolean = false): Type =
    t match {
      case MapType(props, _, _) if reqOnly           => subtype.join(props.filter(_._2.req).keySet.map(Key.asType))
      case MapType(props, kType, _) if props.isEmpty => kType
      case MapType(props, kType, _)                  => subtype.join(kType, UnionType(props.keySet.map(Key.asType)))
    }

  def getValType(t: MapType): Type =
    subtype.join(t.vType, t.props.values.map(_.tp))

  def getValType(key: Key, t: MapType): Type =
    t.props.get(key).map(_.tp).getOrElse {
      // key represents a literal type
      // so we can use subtyping for testing non-empty intersection
      if (subtype.subType(Key.asType(key), t.kType))
        t.vType
      else
        NoneType
    }

  def withRequiredProp(k: Key, t: MapType): Option[MapType] =
    t.props.get(k) match {
      case Some(Prop(_, tp)) => Some(t.copy(props = t.props.updated(k, Prop(req = true, tp))))
      case None if subtype.subType(Key.asType(k), t.kType) =>
        Some(t.copy(props = t.props.updated(k, Prop(req = true, t.vType))))
      case _ => None
    }

  def selectKeys(reqKeyT: Type, optKeyT: Type, mapT: MapType): Type = {
    val selectProps = mapT.props.collect {
      case (key, Prop(true, tp)) if subtype.subType(Key.asType(key), reqKeyT) => (key, Prop(req = true, tp))
      case (key, Prop(_, tp)) if subtype.subType(Key.asType(key), optKeyT)    => (key, Prop(req = false, tp))
    }
    MapType(selectProps, meet(mapT.kType, optKeyT), mapT.vType)
  }

  private def extractListElem(t: Type): List[Type] =
    t match {
      case DynamicType =>
        List(DynamicType)
      case BoundedDynamicType(bound) =>
        extractListElem(bound).map(BoundedDynamicType)
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

  def extractFunTypes(ty: Type, arity: Int): Set[FunType] = ty match {
    case DynamicType =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType))
    case BoundedDynamicType(bound) =>
      extractFunTypes(bound, arity).map(ft =>
        FunType(ft.forall, ft.argTys.map(BoundedDynamicType), BoundedDynamicType(ft.resTy))
      )
    case AnyFunType =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), DynamicType))
    case ft: FunType =>
      if (ft.argTys.size == arity) Set(ft)
      else Set()
    case AnyArityFunType(resTy) =>
      Set(FunType(List.empty, List.fill(arity)(DynamicType), resTy))
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
      case BoundedDynamicType(bound) =>
        asTupleTypeAux(bound, arity).map(tt => TupleType(tt.argTys.map(BoundedDynamicType)))
      case AnyType | VarType(_) =>
        List(TupleType(List.fill(arity)(AnyType)))
      case r: RecordType if arity > 0 =>
        val rec = util.getRecord(r.module, r.name)
        val recFieldTypes = rec match {
          case Some(recDecl) =>
            recDecl.fields.values.toList.map(_.tp)
          case None =>
            List.fill(arity - 1)(DynamicType)
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
            List.fill(arity - 1)(DynamicType)
        }
        val recArity = recFieldTypes.size + 1
        if (arity == recArity) {
          List(TupleType(AtomLitType(r.recType.name) :: recFieldTypes))
        } else
          List()
      case AnyTupleType =>
        List(TupleType(List.fill(arity)(DynamicType)))
      case tt: TupleType if tt.argTys.size == arity => List(tt)
      case UnionType(tys)                           => tys.flatMap(asTupleTypeAux(_, arity)).toList
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asTupleTypeAux(body, arity)
      case _ => List()
    }

  def filterTupleType(t: Type, elemIndex: Int, elemTy: Type): Type =
    if (elemIndex >= 1)
      filterTupleTypeAux(t, elemIndex - 1, elemTy)
    else
      NoneType

  private def filterTupleTypeAux(t: Type, elemIndex: Int, elemTy: Type): Type =
    t match {
      case _: OpaqueType =>
        t
      case DynamicType =>
        t
      case BoundedDynamicType(bound) =>
        BoundedDynamicType(filterTupleTypeAux(bound, elemIndex, elemTy))
      case AnyType | VarType(_) =>
        t
      case AnyTupleType =>
        t
      case tt: TupleType if isTupleElem(tt, elemIndex, elemTy) =>
        t
      case r: RecordType =>
        recordToTuple(r) match {
          case Some(tt) if isTupleElem(tt, elemIndex, elemTy) =>
            t
          case _ =>
            NoneType
        }
      case r: RefinedRecordType =>
        refinedRecordToTuple(r) match {
          case Some(tt) if isTupleElem(tt, elemIndex, elemTy) =>
            t
          case _ =>
            NoneType
        }
      case UnionType(tys) =>
        UnionType(tys.map(filterTupleTypeAux(_, elemIndex, elemTy)))
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        filterTupleTypeAux(body, elemIndex, elemTy)
      case _ => NoneType
    }

  private def isTupleElem(tupleType: TupleType, elemIndex: Int, elemTy: Type): Boolean = {
    val TupleType(ts) = tupleType
    (elemIndex < ts.length) && subtype.subType(ts(elemIndex), elemTy)
  }

  /**
  * Given a type (required to be a subtype of `AnyTupleType`) and an index, returns the type of the tuple element at
  * the index wrapped in a `Right`. If the index can be possibly out of bounds (in at least one of the options in a
  * union) the function returns `Left(tupLen)`, where `tupLen` is the minimum index value for which this operation would
  * type check.
  */
  def getTupleElement(t: Type, idx: Int): Either[Int, Type] = t match {
    case NoneType =>
      Right(NoneType)
    case DynamicType =>
      Right(DynamicType)
    case AnyTupleType =>
      Right(DynamicType)
    case BoundedDynamicType(t) if subtype.subType(t, AnyTupleType) =>
      Right(BoundedDynamicType(getTupleElement(t, idx).getOrElse(NoneType)))
    case BoundedDynamicType(t) =>
      Right(BoundedDynamicType(NoneType))
    case TupleType(elemTys) if idx >= 1 && idx <= elemTys.length =>
      Right(elemTys(idx - 1))
    case TupleType(elemTys) =>
      Left(elemTys.length)
    case r: RecordType =>
      recordToTuple(r) match {
        case Some(tupTy) => getTupleElement(tupTy, idx)
        case None        => Right(DynamicType)
      }
    case r: RefinedRecordType =>
      refinedRecordToTuple(r) match {
        case Some(tupTy) => getTupleElement(tupTy, idx)
        case None        => Right(DynamicType)
      }
    case UnionType(tys) =>
      val res = tys.map(getTupleElement(_, idx)).foldLeft[Either[Int, Set[Type]]](Right(Set.empty)) {
        case (Right(accTy), Right(elemTy)) => Right(accTy + elemTy)
        case (Left(n1), Left(n2))          => Left(n1.min(n2))
        case (Left(n1), _)                 => Left(n1)
        case (_, Left(n2))                 => Left(n2)
      }
      res.map { optionTys => UnionType(util.flattenUnions(UnionType(optionTys)).toSet) }
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      getTupleElement(body, idx)
    case _ =>
      throw new IllegalStateException()
  }

  /**
  * Given a type (required to be a subtype of `AnyTupleType`), returns the union of all its element types.
  */
  def getAllTupleElements(t: Type): Type = t match {
    case NoneType =>
      NoneType
    case DynamicType =>
      DynamicType
    case AnyTupleType =>
      DynamicType
    case BoundedDynamicType(t) if subtype.subType(t, AnyTupleType) =>
      BoundedDynamicType(getAllTupleElements(t))
    case BoundedDynamicType(t) =>
      BoundedDynamicType(NoneType)
    case TupleType(elemTys) =>
      UnionType(elemTys.toSet)
    case r: RecordType =>
      recordToTuple(r) match {
        case Some(tupTy) => getAllTupleElements(tupTy)
        case None        => DynamicType
      }
    case r: RefinedRecordType =>
      refinedRecordToTuple(r) match {
        case Some(tupTy) => getAllTupleElements(tupTy)
        case None        => DynamicType
      }
    case UnionType(tys) =>
      UnionType(util.flattenUnions(UnionType(tys.map(getAllTupleElements))).toSet)
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      getAllTupleElements(body)
    case _ =>
      throw new IllegalStateException()
  }

  private def recordToTuple(r: RecordType): Option[TupleType] =
    refinedRecordToTuple(RefinedRecordType(r, Map()))

  private def refinedRecordToTuple(r: RefinedRecordType): Option[TupleType] =
    util.getRecord(r.recType.module, r.recType.name).map { recDecl =>
      val elemTys = AtomLitType(r.recType.name) :: recDecl.fields.map(f => r.fields.getOrElse(f._1, f._2.tp)).toList
      TupleType(elemTys)
    }

  def adjustMapType(mapType: MapType, keyT: Type, valT: Type): MapType =
    asKeys(keyT) match {
      case Some(keys) if keys.size == 1 =>
        MapType(mapType.props.updated(keys.head, Prop(req = true, valT)), mapType.kType, mapType.vType)
      case Some(keys) =>
        keys.foldLeft(mapType) { case (mapType, key) =>
          val props = mapType.props.updatedWith(key) {
            case Some(prop) => Some(Prop(prop.req, subtype.join(valT, prop.tp)))
            case None       => Some(Prop(req = false, valT))
          }
          MapType(props, mapType.kType, mapType.vType)
        }
      case None if subtype.isDynamicType(mapType.kType) && subtype.isDynamicType(mapType.vType) =>
        mapType
      case None =>
        MapType(mapType.props, subtype.join(mapType.kType, keyT), subtype.join(mapType.vType, valT))
    }

  def setAllFieldsOptional(mapType: MapType, newValTy: Option[Type] = None): Type =
    MapType(
      mapType.props.map { case (key, Prop(_, tp)) => (key, Prop(req = false, newValTy.getOrElse(tp))) },
      mapType.kType,
      newValTy.getOrElse(mapType.vType),
    )

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
      case BoundedDynamicType(_) =>
        BoundedDynamicType(field.tp)
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

  // Recursion is sound since we don't unfold under constructors
  def asKeys(t: Type): Option[Set[Key]] =
    t match {
      case BoundedDynamicType(bound) =>
        asKeys(bound)
      case UnionType(ts) =>
        ts.foldLeft[Option[Set[Key]]](Some(Set())) { (acc, ty) =>
          acc.flatMap(keys => asKeys(ty).map(keys2 => keys ++ keys2))
        }
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asKeys(body)
      case NoneType => Some(Set())
      case _        => Key.fromType(t).map(Set(_))
    }

  private def mergeMaps(s1: MapType, s2: MapType, inOrder: Boolean): MapType = {
    MapType(
      (s1.props.keySet ++ s2.props.keySet).map { key =>
        val prop1 = s1.props.getOrElse(key, Prop(req = false, NoneType))
        val prop2 = s2.props.getOrElse(key, Prop(req = false, NoneType))
        val req = (inOrder && (prop1.req || prop2.req)) || (prop1.req && prop2.req)
        val tp = if (inOrder && prop2.req) prop2.tp else subtype.join(prop1.tp, prop2.tp)
        (key -> Prop(req, tp))
      }.toMap,
      subtype.join(s1.kType, s2.kType),
      subtype.join(s1.vType, s2.vType),
    )
  }

  def joinAndMergeMaps(tys: Iterable[Type], inOrder: Boolean = false): Type = {
    val (maps, notMaps) = tys.partition {
      case m: MapType => true
      case _          => false
    }
    val joinedNotMaps = subtype.join(notMaps)
    val mapsCoerced = maps.collect { case s: MapType => s }
    if (mapsCoerced.isEmpty) {
      joinedNotMaps
    } else {
      subtype.join(
        mapsCoerced.tail.foldLeft(mapsCoerced.head)((acc, map) => mergeMaps(acc, map, inOrder)),
        joinedNotMaps,
      )
    }
  }
}
