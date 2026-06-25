/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec
import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Cons, Expr, Tuple}
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.Types._

final class CustomReturn(pipelineContext: PipelineContext) {
  private lazy val subtype = pipelineContext.subtype

  private type ReturnHandler = (List[Expr], List[Type], Type) => Type

  private lazy val custom: Map[RemoteId, ReturnHandler] = Map(
    RemoteId("erlang", "abs", 1) -> { (_, argTys, resTy) =>
      val List(argTy) = argTys
      if (subtype.subType(argTy, numberType)) argTy else resTy
    },
    RemoteId("erlang", "min", 2) -> { (_, argTys, resTy) =>
      val List(argTy1, argTy2) = argTys
      if (subtype.isNoneType(argTy1) || subtype.isNoneType(argTy2))
        resTy
      else if (
        subtype.gradualSubType(argTy1, IntegerType) &&
        subtype.gradualSubType(argTy2, UnionType(Set(IntegerType, AtomType)))
      )
        IntegerType
      else if (
        subtype.gradualSubType(argTy2, IntegerType) &&
        subtype.gradualSubType(argTy1, UnionType(Set(IntegerType, AtomType)))
      )
        IntegerType
      else if (
        subtype.gradualSubType(argTy1, FloatType) &&
        subtype.gradualSubType(argTy2, UnionType(Set(FloatType, AtomType)))
      )
        FloatType
      else if (
        subtype.gradualSubType(argTy2, FloatType) &&
        subtype.gradualSubType(argTy1, UnionType(Set(FloatType, AtomType)))
      )
        FloatType
      else if (
        subtype.gradualSubType(argTy1, numberType) &&
        subtype.gradualSubType(argTy2, UnionType(Set(numberType, AtomType)))
      )
        numberType
      else if (
        subtype.gradualSubType(argTy2, numberType) &&
        subtype.gradualSubType(argTy1, UnionType(Set(numberType, AtomType)))
      )
        numberType
      else resTy
    },
    RemoteId("lists", "sum", 1) -> { (_, argTys, resTy) =>
      val List(lTy) = argTys
      if (subtype.subType(lTy, ListType(IntegerType)))
        IntegerType
      else
        resTy
    },
    RemoteId("re", "replace", 4) -> { (args, _, resTy) =>
      args match {
        case List(_, _, _, options) =>
          findReturn(options) match {
            case Some("binary") =>
              BinaryType
            case Some("list") =>
              stringType
            case _ => resTy
          }
        case _ => resTy
      }
    },
  )

  def isCustomReturn(remoteId: RemoteId): Boolean =
    custom.contains(remoteId)

  def customizeResultType(remoteId: RemoteId, args: List[Expr], argTys: List[Type], resTy: Type): Type =
    custom(remoteId)(args, argTys, resTy)

  @tailrec
  private def findReturn(optionsArg: Expr): Option[String] =
    optionsArg match {
      case Cons(head, tail) =>
        head match {
          case Tuple(List(AtomLit("return"), AtomLit(returnType))) =>
            return Some(returnType)
          case _ =>
        }
        findReturn(tail)
      case _ =>
        None
    }
}
