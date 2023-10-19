/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.config

// Expands spec constraints, applies rewrite rules, and validates
private class Expand(module: String) {

  def expandFunSpec(funSpec: ExternalFunSpec): Either[InvalidFunSpec, ExternalFunSpec] = {
    try {
      Right(funSpec.copy(types = funSpec.types.map(expandCft))(funSpec.pos))
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        Left(InvalidFunSpec(funSpec.id, e)(funSpec.pos))
    }
  }

  private def expandCft(cft: ConstrainedFunType): ConstrainedFunType = {
    val ft @ FunExtType(args, res) =
      if (cft.constraints.isEmpty) cft.ty
      else {
        val f @ FunExtType(args, res) = cft.ty
        throwIfMultiplyConstrainedTyVar(cft)
        val subst = cft.constraints.map(c => c.tVar -> c.ty).toMap
        val args1 = args.map(expandConstraints(_, subst, Set.empty))
        val res1 = expandConstraints(res, subst, Set.empty)
        FunExtType(args1, res1)(f.pos)
      }
    val ft1 = FunExtType(args.map(expand), expand(res))(ft.pos)
    ConstrainedFunType(ft1, List.empty)(cft.pos)
  }

  def expandCallback(cb: ExternalCallback): Either[InvalidFunSpec, ExternalCallback] = {
    try {
      Right(cb.copy(types = cb.types.map(expandCft))(cb.pos))
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        Left(InvalidFunSpec(cb.id, e)(cb.pos))
    }
  }

  def expandTypeDecl(decl: ExternalTypeDecl): Either[InvalidTypeDecl, ExternalTypeDecl] = {
    try {
      validateTypeVars(decl.pos, decl.body, decl.params)
      Right(decl.copy(body = expand(decl.body))(decl.pos))
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        Left(InvalidTypeDecl(decl.id, e)(decl.pos))
    }
  }

  def expandOpaqueDecl(decl: ExternalOpaqueDecl): Either[InvalidTypeDecl, ExternalOpaqueDecl] = {
    try {
      validateTypeVars(decl.pos, decl.body, decl.params)
      Right(decl.copy(body = expand(decl.body))(decl.pos))
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        Left(InvalidTypeDecl(decl.id, e)(decl.pos))
    }
  }

  private def validateTypeVars(declPos: Pos, body: ExtType, params: List[String]): Unit = {
    val tyVars = ExternalTypeVars.collectTyVars(body)
    throwIfRepeatedTypeParam(declPos, params)
    throwIfUnboundTypeVar(declPos, params, tyVars)
  }

  private def throwIfRepeatedTypeParam(declPos: Pos, params: List[String]): Unit = {
    var names = Set.empty[String]
    for (name <- params) {
      if (names(name)) throw InvalidDiagnostics.RepeatedTyVarInTyDecl(declPos, name)
      names += name
    }
  }

  private def throwIfMultiplyConstrainedTyVar(cft: ConstrainedFunType): Unit = {
    var names = Set.empty[String]
    for (name <- cft.constraints.map(_.tVar)) {
      if (names(name)) throw InvalidDiagnostics.TyVarWithMultipleConstraints(cft.pos, name)
      names += name
    }
  }

  private def throwIfUnboundTypeVar(declPos: Pos, params: List[String], tyVars: List[VarExtType]): Unit =
    tyVars.find(tv => !params.contains(tv.name)) match {
      case Some(tyVar) =>
        throw InvalidDiagnostics.UnboundTyVarInTyDecl(declPos, tyVar.name)
      case None => ()
    }

  def expandRecDecl(decl: ExternalRecDecl): Either[InvalidRecDecl, ExternalRecDecl] =
    try {
      Right(decl.copy(fields = decl.fields.map(expandRecField))(decl.pos))
    } catch {
      case e: InvalidDiagnostics.Invalid =>
        Left(InvalidRecDecl(decl.name, e)(decl.pos))
    }

  private def expand(t: ExtType): ExtType =
    t match {
      case LocalExtType(Id(n, arity), params) =>
        val id = RemoteId(module, n, arity)
        val expandedParams = params.map(expand)
        RemoteExtType(id, expandedParams)(t.pos)
      case RemoteExtType(id, params) =>
        val localId = Id(id.name, id.arity)
        val isDefined = Db.getTypeIds(id.module)(localId)
        val isExported = Db.getExportedTypeIds(id.module)(localId)
        if (!isDefined)
          throw InvalidDiagnostics.UnknownId(t.pos, id)
        if (id.module != module && !isExported)
          throw InvalidDiagnostics.NonExportedId(t.pos, id)
        val expandedParams = params.map(expand)
        RemoteExtType(id, expandedParams)(t.pos)
      case FunExtType(argTys, resTy) =>
        FunExtType(argTys.map(expand), expand(resTy))(t.pos)
      case AnyArityFunExtType(resTy) =>
        AnyArityFunExtType(expand(resTy))(t.pos)
      case TupleExtType(argTys) =>
        TupleExtType(argTys.map(expand))(t.pos)
      case ListExtType(t) =>
        ListExtType(expand(t))(t.pos)
      case AnyListExtType() =>
        val eqwalizerDynamic = RemoteExtType(RemoteId("eqwalizer", "dynamic", 0), List.empty)(t.pos)
        ListExtType(eqwalizerDynamic)(t.pos)
      case UnionExtType(tys) =>
        UnionExtType(tys.map(expand))(t.pos)
      case MapExtType(props) =>
        if (props.exists(isBadProp) && config.approximateComplexTypes) {
          val eqwalizerDynamic = RemoteExtType(RemoteId("eqwalizer", "dynamic", 0), List.empty)(t.pos)
          MapExtType(List(OptExtProp(eqwalizerDynamic, eqwalizerDynamic)(t.pos)))(t.pos)
        } else
          MapExtType(props.map(expandProp))(t.pos)
      case AnyMapExtType() =>
        val eqwalizerDynamic = RemoteExtType(RemoteId("eqwalizer", "dynamic", 0), List.empty)(t.pos)
        MapExtType(List(OptExtProp(eqwalizerDynamic, eqwalizerDynamic)(t.pos)))(t.pos)
      case RecordRefinedExtType(name, refinedFields) =>
        RecordRefinedExtType(name, refinedFields.map(expandRefinedRecordField))(t.pos)
      case _: VarExtType | _: BuiltinExtType | _: IntLitExtType | _: AtomLitExtType | _: RecordExtType | _: UnOpType |
          _: BinOpType =>
        t
    }

  private def isBadProp(prop: ExtProp): Boolean =
    prop match {
      case ReqBadExtProp(_, _) => true
      case OptBadExtProp(_, _) => true
      case _                   => false
    }

  private def expandProp(prop: ExtProp): ExtProp =
    prop match {
      case ReqExtProp(key, tp) =>
        ReqExtProp(expand(key), expand(tp))(prop.pos)
      case ReqBadExtProp(_, _) =>
        throw InvalidDiagnostics.BadMapKey(prop.pos)
      case OptExtProp(key, tp) =>
        OptExtProp(expand(key), expand(tp))(prop.pos)
      case OptBadExtProp(_, _) =>
        throw InvalidDiagnostics.BadMapKey(prop.pos)
    }

  private def expandRefinedRecordField(field: RefinedField): RefinedField =
    RefinedField(field.label, expand(field.ty))

  private def expandConstraints(t: ExtType, s: Map[String, ExtType], stack: Set[String]): ExtType =
    t match {
      case LocalExtType(Id(n, arity), params) =>
        val id = RemoteId(module, n, arity)
        RemoteExtType(id, params.map(expandConstraints(_, s, stack)))(t.pos)
      case RemoteExtType(id, params) =>
        RemoteExtType(id, params.map(expandConstraints(_, s, stack)))(t.pos)
      case FunExtType(args, resType) =>
        FunExtType(args.map(expandConstraints(_, s, stack)), expandConstraints(resType, s, stack))(t.pos)
      case AnyArityFunExtType(resTy) =>
        AnyArityFunExtType(expandConstraints(resTy, s, stack))(t.pos)
      case TupleExtType(params) =>
        TupleExtType(params.map(expandConstraints(_, s, stack)))(t.pos)
      case ListExtType(et) =>
        ListExtType(expandConstraints(et, s, stack))(t.pos)
      case UnionExtType(params) =>
        UnionExtType(params.map(expandConstraints(_, s, stack)))(t.pos)
      case MapExtType(props) =>
        if (props.exists(isBadProp) && config.approximateComplexTypes) {
          val eqwalizerDynamic = RemoteExtType(RemoteId("eqwalizer", "dynamic", 0), List.empty)(t.pos)
          MapExtType(List(OptExtProp(eqwalizerDynamic, eqwalizerDynamic)(t.pos)))(t.pos)
        } else
          MapExtType(props.map(expandProp(_, s, stack)))(t.pos)
      case RecordRefinedExtType(name, refinedFields) =>
        RecordRefinedExtType(name, refinedFields.map(expandRefinedRecordField(_, s, stack)))(t.pos)
      case v: VarExtType =>
        if (stack(v.name))
          throw InvalidDiagnostics.RecursiveConstraint(v.pos, v.name)
        else
          s.get(v.name) match {
            case Some(tp) => expandConstraints(tp, s, stack + v.name)
            case None     => t
          }
      case _: BuiltinExtType | _: IntLitExtType | _: AtomLitExtType | _: RecordExtType | _: AnyMapExtType |
          _: UnOpType | _: BinOpType | _: AnyListExtType =>
        t
    }

  private def expandProp(prop: ExtProp, s: Map[String, ExtType], stack: Set[String]): ExtProp =
    prop match {
      case ReqExtProp(key, tp) =>
        ReqExtProp(expandConstraints(key, s, stack), expandConstraints(tp, s, stack))(prop.pos)
      case ReqBadExtProp(_, _) =>
        throw InvalidDiagnostics.BadMapKey(prop.pos)
      case OptExtProp(key, tp) =>
        OptExtProp(expandConstraints(key, s, stack), expandConstraints(tp, s, stack))(prop.pos)
      case OptBadExtProp(_, _) =>
        throw InvalidDiagnostics.BadMapKey(prop.pos)
    }

  private def expandRefinedRecordField(field: RefinedField, s: Map[String, ExtType], stack: Set[String]): RefinedField =
    RefinedField(field.label, expandConstraints(field.ty, s, stack))

  private def expandRecField(field: ExternalRecField): ExternalRecField =
    field.copy(tp = field.tp.map(expand))
}
