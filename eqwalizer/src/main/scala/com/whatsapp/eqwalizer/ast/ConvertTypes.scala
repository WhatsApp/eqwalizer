/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._

class ConvertTypes(module: String) {
  def convertSpec(spec: ExternalFunSpec): FunSpec = {
    val ExternalFunSpec(id, types) = spec
    val ft = convertCft(types.head)
    FunSpec(id, ft)(spec.pos)
  }

  def convertOverloadedSpec(spec: ExternalFunSpec): OverloadedFunSpec = {
    val ExternalFunSpec(id, types) = spec
    val funTys = types.map(convertCft)
    OverloadedFunSpec(id, funTys)(spec.pos)
  }

  def convertCallback(cb: ExternalCallback): Callback = {
    val ExternalCallback(id, cfts) = cb
    val tys = cfts.map(convertCft)
    Callback(id, tys)(cb.pos)
  }

  private def convertCft(cft: ConstrainedFunType): FunType = {
    val ConstrainedFunType(extFunTy, constraints) = cft
    assert(constraints.isEmpty, "Expand phase should inline constraints")

    val varNames = collectVarNames(extFunTy)
    val sub = varNames.zipWithIndex.toMap
    val ft = convertType(sub, extFunTy)(ctx = None).asInstanceOf[FunType]
    ft.copy(forall = sub.values.toList)
  }

  def convertRecDecl(extRecDecl: ExternalRecDecl): InternalForm = {
    try {
      val ExternalRecDecl(name, fields) = extRecDecl
      val fields1 = fields.map(convertRecField(_)(extRecDecl))
      val refinable = fields1.exists { case RecField(_, _, _, refinable) => refinable }
      RecDecl(name, fields1, refinable)(extRecDecl.pos)
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        InvalidConvertTypeInRecDecl(extRecDecl.name, e)(extRecDecl.pos)
    }
  }

  private def convertRecField(extRecField: ExternalRecField)(implicit ctx: ExternalRecDecl): RecField = {
    val ExternalRecField(name, ty, defaultValue) = extRecField
    val refinable =
      ty match {
        case Some(RemoteExtType(id, args)) if id == RemoteId("eqwalizer", "refinable", 1) => true
        case _                                                                            => false
      }
    RecField(name, ty.map(convertType(Map.empty, _)(Some(ctx))), defaultValue, refinable)
  }

  def convertTypeDecl(extDecl: ExternalTypeDecl): InternalForm =
    try {
      val params = extDecl.params.zipWithIndex.map { case (n, i) => VarType(i)(n) }
      val sub = params.map(v => v.name -> v.n).toMap
      val body = convertType(sub, extDecl.body)(None)
      TypeDecl(extDecl.id, params, body)(extDecl.pos)
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        InvalidTypeDecl(extDecl.id, e)(extDecl.pos)
    }

  def convertOpaqueDeclPublic(extDecl: ExternalOpaqueDecl): OpaqueTypeDecl =
    OpaqueTypeDecl(extDecl.id)(extDecl.pos)

  def convertOpaquePrivate(extDecl: ExternalOpaqueDecl): InternalForm =
    try {
      val params = extDecl.params.zipWithIndex.map { case (n, i) => VarType(i)(n) }
      val sub = params.map(v => v.name -> v.n).toMap
      val body = convertType(sub, extDecl.body)(None)
      TypeDecl(extDecl.id, params, body)(extDecl.pos)
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        InvalidTypeDecl(extDecl.id, e)(extDecl.pos)
    }

  def convertType(s: Map[String, Int], extTy: ExtType)(implicit ctx: Option[ExternalRecDecl]): Type = {
    def c(eTy: ExtType): Type = convertType(s, eTy)
    extTy match {
      case AtomLitExtType(atom)      => AtomLitType(atom)
      case FunExtType(argTys, resTy) =>
        // we interpret all specs as prenex form: all variables are bound
        // all the way on the left:
        // foo(X, fun((X) -> Y) -> ... is foo[X, Y](X, fun((X) -> Y)) -> ...
        FunType(Nil, argTys.map(c), c(resTy))
      case AnyArityFunExtType(_) =>
        AnyFunType
      case TupleExtType(argTys) => TupleType(argTys.map(c))
      case ListExtType(t)       => ListType(c(t))
      case UnionExtType(tys)    => UnionType(tys.toSet.map(c))
      case RemoteExtType(id, argTys) if id == RemoteId("eqwalizer", "refinable", 1) && argTys.size == 1 =>
        c(argTys.head)
      case RemoteExtType(id, argTys) =>
        val m = id.module
        RemoteType(id, argTys.map(c))
      case VarExtType(name) =>
        s.get(name) match {
          case Some(ty) =>
            VarType(ty)(name)
          case None =>
            ctx match {
              case Some(_) =>
                throw InvalidDiagnostics.TypeVarInRecordField(extTy.pos, name)
              // $COVERAGE-OFF$
              case _ => throw new IllegalStateException(s"unexpected $extTy")
              // $COVERAGE-ON$
            }
        }
      case RecordExtType(name) =>
        RecordType(name)(module)
      case RecordRefinedExtType(name, fields) =>
        val rTy = RecordType(name)(module)
        val refinedFields = fields.map(f => f.label -> c(f.ty)).toMap
        RefinedRecordType(rTy, refinedFields)
      case MapExtType(props) =>
        val atomKeyedProps = props.collect {
          case prop @ ReqExtProp(AtomLitExtType(key), _) => (key, prop)
          case prop @ OptExtProp(AtomLitExtType(key), _) => (key, prop)
        }
        val isShape = atomKeyedProps.size == props.size
        if (isShape) ShapeMap(atomKeyedProps.map(toShapeProps(s, _)))
        else DictMap(c(props.head.key), c(props.head.tp))
      case BuiltinExtType(name) =>
        builtinTypes(name)
      case RangeExtType(first, last) =>
        NumberType
      case IntLitExtType(value) =>
        NumberType
      case _: UnOpType | _: BinOpType =>
        NumberType
      // $COVERAGE-OFF$
      case _: LocalExtType | _: AnyMapExtType | _: AnyListExtType =>
        throw new IllegalStateException(s"should have been removed in the expand phase $extTy")
      // $COVERAGE-ON$
    }
  }

  private def toShapeProps(s: Map[String, Int], keyAndProp: (String, ExtProp))(implicit
      ctx: Option[ExternalRecDecl]
  ): Prop = {
    val (key, prop) = keyAndProp
    prop match {
      case ReqExtProp(_, ty) => ReqProp(key, convertType(s, ty))
      case OptExtProp(_, ty) => OptProp(key, convertType(s, ty))
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }

  private def collectVarNames(ty: ExtType): List[String] = ty match {
    case VarExtType(name) =>
      name :: Nil
    case FunExtType(argTys, resTy) =>
      (resTy :: argTys).flatMap(collectVarNames)
    case AnyArityFunExtType(resTy) =>
      collectVarNames(resTy)
    case TupleExtType(argTys) =>
      argTys.flatMap(collectVarNames)
    case UnionExtType(tys) =>
      tys.flatMap(collectVarNames)
    case LocalExtType(_, args) =>
      args.flatMap(collectVarNames)
    case RemoteExtType(_, args) =>
      args.flatMap(collectVarNames)
    case RecordRefinedExtType(_, fields) =>
      fields.flatMap(f => collectVarNames(f.ty))
    case MapExtType(props) =>
      props.flatMap(p => List(p.key, p.tp)).flatMap(collectVarNames)
    case ListExtType(ty) =>
      collectVarNames(ty)
    case _: AtomLitExtType | _: VarExtType | _: RecordExtType | _: BuiltinExtType | _: RangeExtType | _: IntLitExtType |
        _: AnyMapExtType | _: UnOpType | _: BinOpType | _: AnyListExtType =>
      Nil
  }
}
