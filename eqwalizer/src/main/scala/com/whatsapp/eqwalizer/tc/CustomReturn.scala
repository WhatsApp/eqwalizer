/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec

import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Cons, Expr, Tuple}
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.Types.{BinaryType, Type, stringType}

final class CustomReturn(pipelineContext: PipelineContext) {
  private lazy val util = pipelineContext.util
  private lazy val custom: Set[RemoteId] =
    Set(
      RemoteId("re", "replace", 4)
    )

  def isCustomReturn(remoteId: RemoteId): Boolean =
    custom(remoteId)

  def customizeResultType(remoteId: RemoteId, args: List[Expr], argTys: List[Type], resTy: Type): Type =
    remoteId match {
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
