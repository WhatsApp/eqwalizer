package com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec

import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Cons, Expr, Tuple}
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.Types.{BinaryType, Type, stringType}

object CustomReturn {
  private lazy val custom: Set[RemoteId] =
    Set(
      RemoteId("re", "replace", 4)
    )

  def isCustomReturn(remoteId: RemoteId): Boolean =
    custom(remoteId)

  def customizeResultType(remoteId: RemoteId, args: List[Expr], resTy: Type): Type =
    (remoteId, args) match {
      case (RemoteId("re", "replace", 4), List(_, _, _, options)) =>
        findReturn(options) match {
          case Some("binary") =>
            BinaryType
          case Some("list") =>
            stringType
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
