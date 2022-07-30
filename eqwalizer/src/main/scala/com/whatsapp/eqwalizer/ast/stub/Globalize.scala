/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.{Id, RemoteId}

private object Globalize {
  def globalize(module: String, t: ExtType): ExtType =
    t match {
      case FunExtType(args, resType) =>
        FunExtType(args.map(globalize(module, _)), globalize(module, resType))(t.pos)
      case AnyArityFunExtType(resTy) =>
        AnyArityFunExtType(globalize(module, resTy))(t.pos)
      case LocalExtType(Id(n, arity), params) =>
        RemoteExtType(RemoteId(module, n, arity), params.map(globalize(module, _)))(t.pos)
      case RemoteExtType(id, params) =>
        RemoteExtType(id, params.map(globalize(module, _)))(t.pos)
      case TupleExtType(params) =>
        TupleExtType(params.map(globalize(module, _)))(t.pos)
      case ListExtType(et) =>
        ListExtType(globalize(module, et))(t.pos)
      case UnionExtType(params) =>
        UnionExtType(params.map(globalize(module, _)))(t.pos)
      case MapExtType(props) =>
        MapExtType(props.map(globalizeProp(module, _)))(t.pos)
      case RecordRefinedExtType(name, fields) =>
        RecordRefinedExtType(name, fields.map(f => RefinedField(f.label, globalize(module, f.ty))))(t.pos)
      case _: VarExtType | _: BuiltinExtType | _: RangeExtType | _: IntLitExtType | _: AtomLitExtType |
          _: RecordExtType | _: AnyMapExtType | _: UnOpType | _: BinOpType | _: AnyListExtType =>
        t
    }

  def globalizeSpec(module: String, spec: ExternalFunSpec): ExternalFunSpec = {
    val types = spec.types.map(globalizeCft(module, _))
    spec.copy(types = types)(spec.pos)
  }

  def globalizeCallback(module: String, cb: ExternalCallback): ExternalCallback = {
    val types = cb.types.map(globalizeCft(module, _))
    cb.copy(types = types)(cb.pos)
  }

  private def globalizeCft(module: String, cft: ConstrainedFunType): ConstrainedFunType = {
    val ConstrainedFunType(ft @ FunExtType(args, res), constraints) = cft
    ConstrainedFunType(
      FunExtType(args.map(globalize(module, _)), globalize(module, res))(ft.pos),
      constraints.map { case c @ Constraint(v, tp) => Constraint(v, globalize(module, tp))(c.pos) },
    )(cft.pos)
  }

  def globalizeTypeDecl(module: String, decl: ExternalTypeDecl): ExternalTypeDecl =
    decl.copy(body = globalize(module, decl.body))(decl.pos)

  def globalizeOpaqueDecl(module: String, decl: ExternalOpaqueDecl): ExternalOpaqueDecl =
    decl.copy(body = globalize(module, decl.body))(decl.pos)

  def globalizeRecDecl(module: String, decl: ExternalRecDecl): ExternalRecDecl =
    decl.copy(fields = decl.fields.map(globalizeRecField(module, _)))(decl.pos)

  private def globalizeRecField(module: String, field: ExternalRecField): ExternalRecField =
    field.copy(tp = field.tp.map(globalize(module, _)))

  private def globalizeProp(module: String, prop: ExtProp): ExtProp =
    prop match {
      case ReqExtProp(key, tp) =>
        ReqExtProp(globalize(module, key), globalize(module, tp))(prop.pos)
      case ReqBadExtProp(key, tp) =>
        ReqBadExtProp(globalize(module, key), globalize(module, tp))(prop.pos)
      case OptExtProp(key, tp) =>
        OptExtProp(globalize(module, key), globalize(module, tp))(prop.pos)
      case OptBadExtProp(key, tp) =>
        OptBadExtProp(globalize(module, key), globalize(module, tp))(prop.pos)
    }
}
