/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import Types._
import com.whatsapp.eqwalizer.tc.PipelineContext

import scala.annotation.tailrec

case class Show(pipelineContext: Option[PipelineContext]) {
  import Show.NamedType

  def show(tp: Type): String =
    tp match {
      case AtomLitType(atom) =>
        s"'$atom'"
      case FunType(_forall, argTys, resTy) =>
        s"""fun((${argTys.map(show).mkString(", ")}) -> ${show(resTy)})"""
      case AnyArityFunType(resTy) =>
        s"""fun((...) -> ${show(resTy)})"""
      case TupleType(elems) =>
        elems.map(show).mkString("{", ", ", "}")
      case ListType(elemType) =>
        s"""[${show(elemType)}]"""
      case NilType =>
        "[]"
      case UnionType(elemTys) =>
        elemTys.map(show).mkString(" | ")
      case _: RemoteType | _: OpaqueType =>
        val NamedType(rid, argTys) = tp
        val prefix = showRid(rid)
        s"$prefix${argTys.map(show).mkString("(", ", ", ")")}"
      case vt: VarType =>
        vt.name
      case DictMap(kt, vt) =>
        s"#D{${show(kt)} => ${show(vt)}}"
      case AnyType =>
        "term()"
      case AtomType =>
        "atom()"
      case NoneType =>
        "none()"
      case NumberType =>
        "number()"
      case PidType =>
        "pid()"
      case PortType =>
        "port()"
      case ReferenceType =>
        "reference()"
      case BinaryType =>
        "binary()"
      case RecordType(n) =>
        s"#$n{}"
      case RefinedRecordType(r, fields) =>
        pipelineContext.flatMap(_.util.getRecord(r.module, r.name)) match {
          case None => s"#${r.name}{}"
          case Some(recDecl) =>
            fields
              .collect {
                case (name, ty) if recDecl.fields(name).refinable => s"${name} :: ${show(ty)}"
              }
              .mkString(s"#${r.name}{", ", ", "}")
        }
      case ShapeMap(props) =>
        props.sortBy(_.key).map(showProp).mkString("#S{", ", ", "}")
      case AnyTupleType =>
        "tuple()"
      case AnyFunType =>
        "fun()"
      case DynamicType =>
        "dynamic()"
    }

  def showNotSubtype(t1: Type, t2: Type): (String, String) = (t1, t2) match {
    case (ft1: FunType, ft2: FunType) if ft1.forall.size != ft2.forall.size =>
      val cnt1 = ft1.forall.size
      val cnt2 = ft2.forall.size
      def plural(n: Int): String = if (n != 1) "s" else ""
      (
        s"${show(ft1)} with $cnt1 type parameter${plural(cnt1)}",
        s"${show(ft2)} with $cnt2 type parameter${plural(cnt2)}",
      )
    case (NamedType(rid1, argTys1), NamedType(rid2, argTys2)) if rid1.name == rid2.name && rid1.module != rid2.module =>
      val prefix1 = showRid(rid1, forceShowModule = true)
      val prefix2 = showRid(rid2, forceShowModule = true)
      (
        s"$prefix1${argTys1.map(show).mkString("(", ", ", ")")}",
        s"$prefix2${argTys2.map(show).mkString("(", ", ", ")")}",
      )
    case (dt: DictMap, st: ShapeMap) =>
      (s"dict  map ${show(dt)}", s"shape map ${show(st)}")

    case _ =>
      (show(t1), show(t2))
  }

  private def showProp(prop: Prop): String =
    prop match {
      case ReqProp(key, tp) =>
        s"$key := ${show(tp)}"
      case OptProp(key, tp) =>
        s"$key => ${show(tp)}"
    }

  private def showRid(rid: RemoteId, forceShowModule: Boolean = false): String = {
    pipelineContext match {
      case Some(ctx)
          if !forceShowModule &&
            ((rid.module == "erlang" && builtinTypes.contains(rid.name)) ||
              rid.module == ctx.module ||
              (ctx.gradualTyping && rid == RemoteId("eqwalizer", "dynamic", 0))) =>
        rid.name
      case _ => s"${rid.module}:${rid.name}"
    }
  }
}

object Show {

  object NamedType {
    def unapply(ty: Type): Option[(RemoteId, List[Type])] = ty match {
      case RemoteType(rid, argTys) => Some(rid, argTys)
      case OpaqueType(rid, argTys) => Some(rid, argTys)
      case _                       => None
    }
  }

  def show(tp: Type)(implicit pipelineContext: PipelineContext): String =
    Show(Some(pipelineContext)).show(tp)

  def showNotSubtype(t1: Type, t2: Type)(implicit pipelineContext: PipelineContext): (String, String) =
    Show(Some(pipelineContext)).showNotSubtype(t1, t2)

  @tailrec
  private def foldCons(cons: Expr, soFar: List[Expr]): (List[Expr], Option[Expr]) = cons match {
    case Cons(h, tail) => foldCons(tail, h :: soFar)
    case _: NilLit     => (soFar.reverse, None)
    case tail          => (soFar.reverse, Some(tail))
  }

  def show(e: Expr): String = e match {
    case Var(n) =>
      n
    case Exprs.AtomLit(atom) =>
      s"'$atom'"
    case IntLit(value) =>
      value.toString
    case FloatLit() =>
      "float_lit"
    case StringLit(_) =>
      "string_lit"
    case Block(_) =>
      "block_expr"
    case LocalCall(Id(f, _), args) =>
      s"""$f(${args.map(show).mkString(", ")})"""
    case Exprs.RemoteCall(RemoteId(m, t, _), args) =>
      s"""$m:$t(${args.map(show).mkString(", ")})"""
    case Lambda(_) =>
      "fun"
    case DynCall(Var(v), args) =>
      s"$v${args.map(show).mkString("(", ", ", ")")}"
    case DynCall(f, args) =>
      s"(${show(f)})${args.map(show).mkString("(", ", ", ")")}"
    case DynRemoteFun(m, f) =>
      s"${show(m)}:${show(f)}"
    case DynRemoteFunArity(m, f, a) =>
      s"${show(m)}:${show(f)}/${show(a)}"
    case LocalFun(id) =>
      id.toString
    case RemoteFun(id) =>
      id.toString
    case Tuple(elems) =>
      elems.map(show).mkString("{", ", ", "}")
    case c: Cons =>
      foldCons(c, Nil) match {
        case (elems, None) =>
          elems.map(show).mkString("[", ", ", "]")
        case (elems, Some(tail)) =>
          s"[${elems.map(show).mkString(", ")} | ${show(tail)}]"
      }
    case NilLit() =>
      "[]"
    case Match(_, _) =>
      "match_expr"
    case Case(_, _) =>
      "case ..."
    case If(_) =>
      "if ..."
    case UnOp(op, _) =>
      s"$op _"
    case BinOp(op, _, _) =>
      s"_ $op _"
    case Binary(_) =>
      "<<..>>"
    case Catch(_) =>
      "catch"
    case TryCatchExpr(_, _, _) =>
      "try .. catch .."
    case TryOfCatchExpr(_, _, _, _) =>
      "try .. of .. catch .."
    case Receive(_) =>
      "receive .."
    case ReceiveWithTimeout(_, _, _) =>
      "receive .."
    case LComprehension(_, _) =>
      "[ || ]"
    case BComprehension(_, _) =>
      "<< || >>"
    case RecordCreate(recName, _) =>
      s"#$recName{...}"
    case RecordUpdate(_, recName, _) =>
      s"...#$recName{...}"
    case RecordSelect(_, recName, fieldName) =>
      s"...#$recName.$fieldName"
    case RecordIndex(recName, fieldName) =>
      s"#$recName.$fieldName"
    case MapCreate(_) =>
      "#{..}"
    case MapUpdate(_, _) =>
      "..#{..}"
  }
}
