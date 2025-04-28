/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.ast.{Id, RemoteId}

class Util(pipelineContext: PipelineContext) {
  private val module = pipelineContext.module
  private var recordCache: Map[(String, String), Option[RecDecl]] = Map.empty

  def globalFunId(module: String, id: Id): RemoteId = {
    val imports = Db.getImports(module).get
    val hostModule = imports.getOrElse(id, module)
    RemoteId(hostModule, id.name, id.arity)
  }

  def getFunType(module: String, id: Id): FunType =
    getFunType(globalFunId(module, id))

  def getRecord(module: String, name: String): Option[RecDecl] = {
    if (recordCache.contains((module, name))) {
      recordCache((module, name))
    } else {
      val recDecl = Db.getRecord(module, name)
      recordCache += ((module, name) -> recDecl)
      recDecl
    }
  }

  def getFunType(fqn: RemoteId): FunType = {
    Db.getSpec(fqn.module, Id(fqn.name, fqn.arity)).map(_.ty) match {
      case Some(funType) =>
        funType
      case None =>
        val arity = fqn.arity
        FunType(Nil, List.fill(arity)(DynamicType), DynamicType)
    }
  }

  def getOverloadedSpec(fqn: RemoteId): Option[OverloadedFunSpec] =
    Db.getOverloadedSpec(fqn.module, Id(fqn.name, fqn.arity))

  def enterScope(env0: Env, scopeVars: Set[String]): Env = {
    var env = env0
    for {
      v <- scopeVars if !env0.contains(v)
    } env = env.updated(v, AnyType)
    env
  }

  def exitScope(env0: Env, env1: Env, scopeVars: Set[String]): Env = {
    val allVars = env0.keySet ++ scopeVars
    env1.view.filterKeys(allVars).toMap
  }

  def getTypeDeclBody(remoteId: RemoteId, args: List[Type]): Type = {
    remoteId match {
      case RemoteId("eqwalizer", "dynamic", 0) =>
        return DynamicType
      case RemoteId("eqwalizer", "dynamic", 1) =>
        return BoundedDynamicType(args.head)
      case _ =>
    }
    val id = Id(remoteId.name, remoteId.arity)
    def applyType(decl: TypeDecl): Type =
      if (id.arity == 0) decl.body
      else {
        val subst = decl.params.zip(args).map { case (VarType(n), ty) => n -> ty }.toMap
        Subst.subst(subst, decl.body)
      }

    Db
      .getType(remoteId.module, id)
      .map(applyType)
      .get // A non-defined type should fail during stubs validation
  }

  def flattenUnions(ty: Type): List[Type] = ty match {
    case UnionType(tys) =>
      val flatTys = tys.flatMap(flattenUnions) - NoneType
      if (flatTys.isEmpty) List(NoneType) else flatTys.toList
    case _ => List(ty)
  }

  def isFunType(ty: Type, arity: Int): Boolean = ty match {
    case FunType(_, argTys, _) if argTys.size == arity => true
    case DynamicType                                   => true
    case NoneType                                      => true
    case AnyFunType                                    => true
    case AnyArityFunType(_)                            => true
    case RemoteType(rid, argTys) =>
      val body = getTypeDeclBody(rid, argTys)
      isFunType(body, arity)
    case UnionType(tys) =>
      tys.forall(isFunType(_, arity))
    case BoundedDynamicType(_) => true
    case _                     => false
  }
}
