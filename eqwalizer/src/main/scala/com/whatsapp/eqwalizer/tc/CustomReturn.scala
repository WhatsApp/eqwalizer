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
  private lazy val custom: Set[RemoteId] =
    Set(
      RemoteId("erlang", "min", 2),
      RemoteId("re", "replace", 4),
    )

  def isCustomReturn(remoteId: RemoteId): Boolean =
    custom(remoteId)

  def customizeResultType(remoteId: RemoteId, args: List[Expr], argTys: List[Type], resTy: Type): Type =
    remoteId match {
      case RemoteId("erlang", "min", 2) =>
        val List(argTy1, argTy2) = argTys
        if (subtype.isNoneType(argTy1) || subtype.isNoneType(argTy2))
          resTy
        else if (
          subtype.gradualSubType(argTy1, NumberType) &&
          subtype.gradualSubType(argTy2, UnionType(Set(NumberType, AtomType)))
        )
          NumberType
        else if (
          subtype.gradualSubType(argTy2, NumberType) &&
          subtype.gradualSubType(argTy1, UnionType(Set(NumberType, AtomType)))
        )
          NumberType
        else resTy
      case RemoteId("re", "replace", 4) =>
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
      case _ =>
        throw new IllegalStateException()
    }

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
