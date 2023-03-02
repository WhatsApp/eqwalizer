/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{Id, InvalidDiagnostics, RemoteId, Show, TypeVars}
import com.whatsapp.eqwalizer.tc.Subst

import scala.collection.mutable.ListBuffer

private class TypesValid {

  def checkStub(stub: ModuleStub): ModuleStub = {
    val module = stub.module
    val privateOpaquesMut = ListBuffer[TypeDecl]()
    val invalidFormsMut = ListBuffer.from(stub.invalidForms)

    stub.privateOpaques.values.foreach(addPrivateOpaqueDecl(_, privateOpaquesMut, invalidFormsMut))

    stub.copy(
      privateOpaques = privateOpaquesMut.map(decl => decl.id -> decl).toMap,
      invalidForms = invalidFormsMut.toList,
    )
  }

  private def addPrivateOpaqueDecl(
      decl: TypeDecl,
      declsMut: ListBuffer[TypeDecl],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit = {
    expandsToContravariant(decl) match {
      case Some((tv, exps)) =>
        val invalid = toInvalidContravariant(decl, tv, exps)
        invalidsMut.addOne(invalid)
        return
      case None =>
    }
    declsMut.addOne(decl)
  }

  private def toInvalidContravariant(decl: TypeDecl, tvar: VarType, exps: List[Type]): InvalidForm = {
    val show = Show(None)
    val expsStr = exps.map(show.show)
    val diag = InvalidDiagnostics.AliasWithNonCovariantParam(
      decl.pos,
      decl.id.name,
      tvar.name,
      expsStr,
    )
    InvalidTypeDecl(decl.id, diag)(decl.pos)
  }

  private def expandsToContravariant(tDecl: TypeDecl): Option[(VarType, List[Type])] = {
    tDecl.params
      .map(tv => (tv, findContravariantExpansion(tDecl.body, positive = true)(history = List(), tv)))
      .find(_._2.nonEmpty)
  }

  private def findContravariantExpansion(ty: Type, positive: Boolean)(implicit
      history: List[RemoteType],
      tv: VarType,
  ): List[Type] = {
    def findInTys(tys: List[Type], positive: Boolean): List[List[Type]] = {
      tys match {
        case List() => List()
        case ty :: tys2 =>
          findContravariantExpansion(ty, positive) match {
            case List() => findInTys(tys2, positive).map(ty :: _)
            case tyExps => tyExps.map(_ :: tys2)
          }
      }
    }
    def findInProps(props: List[Prop]): List[List[Prop]] = {
      props match {
        case List() => List()
        case (p @ ReqProp(key, tp)) :: props2 =>
          findContravariantExpansion(tp, positive) match {
            case List()   => findInProps(props2).map(p :: _)
            case propExps => propExps.map(ReqProp(key, _) :: props2)
          }
        case (p @ OptProp(key, tp)) :: props2 =>
          findContravariantExpansion(tp, positive) match {
            case List()   => findInProps(props2).map(p :: _)
            case propExps => propExps.map(OptProp(key, _) :: props2)
          }
      }
    }
    ty match {
      case _ if history.contains(ty) =>
        List()
      case _ if !TypeVars.containsVar(ty, tv) =>
        List()
      case VarType(_) if !positive =>
        List(ty)
      case t @ RemoteType(id, argTys) =>
        val history2 = t :: history
        getTypeDeclBody(id, argTys)
          .map(tbody =>
            findContravariantExpansion(tbody, positive)(history2, tv) match {
              case List() => List()
              case exps   => t :: exps
            }
          )
          .getOrElse(List())
      case FunType(forall, argTys, resTy) =>
        findInTys(argTys, !positive) match {
          case List() =>
            findContravariantExpansion(resTy, positive).map(FunType(forall, argTys, _))
          case argExps =>
            argExps.map(FunType(forall, _, resTy))
        }
      case AnyArityFunType(resTy) =>
        findContravariantExpansion(resTy, positive).map(AnyArityFunType)
      case TupleType(elemTys) =>
        findInTys(elemTys, positive).map(TupleType)
      case ListType(elemTy) =>
        findContravariantExpansion(elemTy, positive).map(ListType)
      case UnionType(tys) =>
        findInTys(tys.toList, positive).map(tys => UnionType(tys.toSet))
      case OpaqueType(id, argTys) =>
        findInTys(argTys, positive).map(OpaqueType(id, _))
      case RefinedRecordType(recTy, fields) =>
        val fieldExps = fields.map { case (name, fieldTy) =>
          (name, findContravariantExpansion(fieldTy, positive))
        }
        fieldExps
          .collectFirst {
            case (name, fieldExps) if fieldExps.nonEmpty =>
              fieldExps
                .map(fields.updated(name, _))
                .map(RefinedRecordType(recTy, _))
          }
          .getOrElse(List())
      case DictMap(kType, vType) =>
        findContravariantExpansion(kType, positive) match {
          case List() =>
            findContravariantExpansion(vType, positive).map(DictMap(kType, _))
          case kExps =>
            kExps.map(DictMap(_, vType))
        }
      case ShapeMap(props) =>
        findInProps(props).map(ShapeMap)
      case _ => List()
    }
  }

  private def getTypeDeclBody(remoteId: RemoteId, args: List[Type]): Option[Type] = {
    val id = Id(remoteId.name, remoteId.arity)
    val stub = Db
      .getContractiveModuleStub(remoteId.module)
      .getOrElse(
        throw new IllegalStateException(
          s"Expand phase should validate that all remoteIds point to modules that exist, but found $remoteId"
        )
      )

    def applyType(decl: TypeDecl): Type = {
      val subst = decl.params.zip(args).map { case (VarType(n), ty) => n -> ty }.toMap
      Subst.subst(subst, decl.body)
    }

    stub.types.get(id).map(applyType)
  }
}
