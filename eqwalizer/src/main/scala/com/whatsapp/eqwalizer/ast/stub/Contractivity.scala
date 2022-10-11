/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms.{InvalidForm, InvalidTypeDecl, OpaqueTypeDecl, TypeDecl}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{Id, InvalidDiagnostics, RemoteId}
import com.whatsapp.eqwalizer.tc.Subst

import scala.collection.mutable.ListBuffer

private class Contractivity(module: String) {
  def checkStub(stub: ModuleStub): ModuleStub = {
    val typesMut = ListBuffer[TypeDecl]()
    val privateOpaquesMut = ListBuffer[TypeDecl]()
    val publicOpaquesMut = ListBuffer[OpaqueTypeDecl]()
    val invalidFormsMut = ListBuffer.from(stub.invalidForms)
    stub.types.values.foreach(addTypeDecl(_, typesMut, invalidFormsMut))
    stub.privateOpaques.values.foreach(
      addOpaqueTypeDecl(_, privateOpaquesMut, stub.publicOpaques, publicOpaquesMut, invalidFormsMut)
    )

    stub.copy(
      types = typesMut.map(decl => decl.id -> decl).toMap,
      privateOpaques = privateOpaquesMut.map(decl => decl.id -> decl).toMap,
      publicOpaques = publicOpaquesMut.map(decl => decl.id -> decl).toMap,
      invalidForms = invalidFormsMut.toList,
    )
  }

  private def addTypeDecl(decl: TypeDecl, declsMut: ListBuffer[TypeDecl], invalidsMut: ListBuffer[InvalidForm]): Unit =
    if (!isContractive(decl.body)) invalidsMut.addOne(toInvalid(decl))
    else declsMut.addOne(decl)

  private def addOpaqueTypeDecl(
      decl: TypeDecl,
      declsMut: ListBuffer[TypeDecl],
      opaques: Map[Id, OpaqueTypeDecl],
      opaquesMut: ListBuffer[OpaqueTypeDecl],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit =
    if (!isContractive(decl.body)) invalidsMut.addOne(toInvalid(decl))
    else {
      declsMut.addOne(decl)
      opaquesMut.addOne(opaques(decl.id))
    }

  private def toInvalid(decl: TypeDecl): InvalidForm = {
    val diag = InvalidDiagnostics.NonProductiveRecursiveTypeAlias(decl.pos, decl.id.toString)
    InvalidTypeDecl(decl.id, diag)(decl.pos)
  }

  // algorithm design by @ilyaklyuchnikov, see D30779530 for more information
  private def isContractive(ty: Type): Boolean = isFoldable(ty, history = Nil)

  def isFoldable(ty: Type, history: List[Type]): Boolean = {
    history.foldLeft(false) { (produced, t) =>
      if (produced && t == ty) return true
      produced || isProducer(t)
    }
    val history1 = ty :: history
    ty match {
      case FunType(forall, argTys, resTy) =>
        assert(forall.isEmpty, s"function types in eqWAlizer type aliases are not expected to have nonempty foralls")
        (resTy :: argTys).forall(isFoldable(_, history1))
      case TupleType(argTys) =>
        argTys.forall(isFoldable(_, history1))
      case ListType(t) =>
        isFoldable(t, history1)
      case UnionType(tys) =>
        tys.forall(isFoldable(_, history1))
      case OpaqueType(id, argTys) =>
        argTys.forall(isFoldable(_, history1))
      case DictMap(kType, vType) =>
        isFoldable(kType, history1) && isFoldable(vType, history1)
      case ShapeMap(props) =>
        props.map(_.tp).forall(isFoldable(_, history1))
      case rt: RefinedRecordType =>
        rt.fields.values.forall(isFoldable(_, history1))
      case RemoteType(rid, argTys) =>
        val hasHe = history.exists(HomeomorphicEmbedding.heByCoupling(_, ty))
        !hasHe && (getTypeDeclBody(rid, argTys) match {
          case Some(body) =>
            isFoldable(body, history1)
          case None =>
            true
        })
      case _ =>
        true
    }
  }

  private def isProducer(ty: Type): Boolean = ty match {
    case _: FunType | _: TupleType | _: ListType | _: OpaqueType | _: DictMap | _: ShapeMap | _: RefinedRecordType =>
      true
    case _: RemoteType =>
      false
    case _: UnionType =>
      false
    case _ =>
      // $COVERAGE-OFF$
      throw new IllegalStateException("unreachable: this is a leaf node of the graph and can't wind up in `history`")
    // $COVERAGE-ON$
  }

  private def getTypeDeclBody(remoteId: RemoteId, args: List[Type]): Option[Type] = {
    val id = Id(remoteId.name, remoteId.arity)
    val stub = Db
      .getExpandedModuleStub(remoteId.module)
      .getOrElse(
        // $COVERAGE-OFF$
        throw new IllegalStateException(
          s"Expand phase should validate that all remoteIds point to modules that exist, but found $remoteId"
        )
        // $COVERAGE-ON$
      )

    def applyType(decl: TypeDecl): Type = {
      val subst = decl.params.zip(args).map { case (VarType(n), ty) => n -> ty }.toMap
      Subst.subst(subst, decl.body)
    }

    stub.types.get(id).map(applyType).orElse {
      if (this.module == remoteId.module) {
        stub.privateOpaques.get(id).map(applyType)
      } else {
        stub.publicOpaques.get(id).map(_ => OpaqueType(remoteId, args))
      }
    }
  }
}
