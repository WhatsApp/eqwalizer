/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.Pats._

class Traverse(val listener: AstListener) {

  def traverseForms(forms: List[ExternalForm]): Unit = {
    val module = forms.collectFirst { case Module(m) => m }.get
    val erlFile = forms.collectFirst { case File(f, _) => f }.get
    var currentFile = erlFile
    val traversHeaders = listener.traverseHeaders()
    listener.enterModule(module, erlFile)
    for (form <- forms)
      form match {
        case f: File =>
          currentFile = f.file
          traverseFile(f)
        case c: CompileExportAll =>
          traverseCompileExportAll(c)
        case e: Export =>
          traverseExport(e)
        case f: FunDecl if traversHeaders || currentFile == erlFile =>
          traverseFun(f)
        case fs: ExternalFunSpec if traversHeaders || currentFile == erlFile =>
          traverseFunSpec(fs)
        case td: ExternalTypeDecl =>
          traverseTypeDecl(td)
        case od: ExternalOpaqueDecl =>
          traverseOpaqueDecl(od)
        case rd: ExternalRecDecl =>
          traverseRecDecl(rd)
        case typing: TypingAttribute =>
          traverseTypingAttribute(typing)
        case _ =>
        // skipping other things for now
      }
    listener.exitModule(module)
  }

  private def traverseFunSpec(spec: ExternalFunSpec): Unit = {
    listener.enterFunSpec(spec)
    spec.types.foreach(cft => {
      traverseType(cft.ty)
      cft.constraints.foreach(c => traverseType(c.ty))
    })
    listener.exitFunSpec(spec)
  }

  private def traverseFun(funDecl: FunDecl): Unit = {
    listener.enterFunDecl(funDecl)
    funDecl.clauses.foreach(traverseClause)
    listener.exitFunDecl(funDecl)
  }

  def traverseExpr(expr: Expr): Unit = expr match {
    case v @ Var(_) =>
      listener.enterExpr(v)
      listener.exitExpr(v)
    case al @ AtomLit(_) =>
      listener.enterExpr(al)
      listener.exitExpr(al)
    case il @ IntLit(_) =>
      listener.enterExpr(il)
      listener.exitExpr(il)
    case fl @ FloatLit() =>
      listener.enterExpr(fl)
      listener.exitExpr(fl)
    case b @ Block(body) =>
      listener.enterExpr(b)
      body.exprs.foreach(traverseExpr)
      listener.exitExpr(b)
    case m @ Match(pat, expr) =>
      listener.enterExpr(m)
      traversePat(pat)
      traverseExpr(expr)
      listener.exitExpr(m)
    case t @ Tuple(elems) =>
      listener.enterExpr(t)
      elems.foreach(traverseExpr)
      listener.exitExpr(t)
    case sl @ StringLit(_) =>
      listener.enterExpr(sl)
      listener.exitExpr(sl)
    case nl @ Exprs.NilLit() =>
      listener.enterExpr(nl)
      listener.exitExpr(nl)
    case c @ Cons(h, t) =>
      listener.enterExpr(c)
      traverseExpr(h)
      traverseExpr(t)
      listener.exitExpr(c)
    case c @ Case(expr, clauses) =>
      listener.enterExpr(c)
      traverseExpr(expr)
      clauses.foreach(traverseClause)
      listener.exitExpr(c)
    case i @ If(clauses) =>
      listener.enterExpr(i)
      clauses.foreach(traverseClause)
      listener.exitExpr(i)
    case lc @ LocalCall(_, args) =>
      listener.enterExpr(lc)
      args.foreach(traverseExpr)
      listener.exitExpr(lc)
    case dc @ DynCall(f, args) =>
      listener.enterExpr(dc)
      traverseExpr(f)
      args.foreach(traverseExpr)
      listener.exitExpr(dc)
    case rc @ RemoteCall(_, args) =>
      listener.enterExpr(rc)
      args.foreach(traverseExpr)
      listener.exitExpr(rc)
    case lf @ LocalFun(_) =>
      listener.enterExpr(lf)
      listener.exitExpr(lf)
    case rf @ RemoteFun(_) =>
      listener.enterExpr(rf)
      listener.exitExpr(rf)
    case drf @ DynRemoteFun(_, _) =>
      listener.enterExpr(drf)
      listener.exitExpr(drf)
    case drf @ DynRemoteFunArity(_, _, _) =>
      listener.enterExpr(drf)
      listener.exitExpr(drf)
    case lam @ Lambda(clauses) =>
      listener.enterExpr(lam)
      clauses.foreach(traverseClause)
      listener.exitExpr(lam)
    case uo @ UnOp(_, arg) =>
      listener.enterExpr(uo)
      traverseExpr(arg)
      listener.exitExpr(uo)
    case bo @ BinOp(_, arg1, arg2) =>
      listener.enterExpr(bo)
      traverseExpr(arg1)
      traverseExpr(arg2)
      listener.exitExpr(bo)
    case lc @ LComprehension(template, qualifiers) =>
      listener.enterExpr(lc)
      traverseExpr(template)
      qualifiers.foreach(traverseQualifier)
      listener.exitExpr(lc)
    case bc @ BComprehension(template, qualifiers) =>
      listener.enterExpr(bc)
      traverseExpr(template)
      qualifiers.foreach(traverseQualifier)
      listener.exitExpr(bc)
    case mc @ MComprehension(kTemplate, vTemplate, qualifiers) =>
      listener.enterExpr(mc)
      traverseExpr(kTemplate)
      traverseExpr(vTemplate)
      qualifiers.foreach(traverseQualifier)
      listener.exitExpr(mc)
    case b @ Binary(elems) =>
      listener.enterExpr(b)
      elems.foreach(traverseBinaryElem)
      listener.exitExpr(b)
    case c @ Catch(expr) =>
      listener.enterExpr(c)
      traverseExpr(expr)
      listener.exitExpr(c)
    case t @ TryCatchExpr(tryBody, catchClauses, afterBody) =>
      listener.enterExpr(t)
      tryBody.exprs.foreach(traverseExpr)
      catchClauses.foreach(traverseClause)
      afterBody.foreach(body => body.exprs.foreach(traverseExpr))
      listener.exitExpr(t)
    case t @ TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
      listener.enterExpr(t)
      tryBody.exprs.foreach(traverseExpr)
      tryClauses.foreach(traverseClause)
      catchClauses.foreach(traverseClause)
      afterBody.foreach(body => body.exprs.foreach(traverseExpr))
      listener.exitExpr(t)
    case r @ Receive(clauses) =>
      listener.enterExpr(r)
      clauses.foreach(traverseClause)
      listener.exitExpr(r)
    case r @ ReceiveWithTimeout(clauses, timeout, timeoutBody) =>
      listener.enterExpr(r)
      clauses.foreach(traverseClause)
      traverseExpr(timeout)
      timeoutBody.exprs.foreach(traverseExpr)
      listener.exitExpr(r)
    case rc @ RecordCreate(_, fields) =>
      listener.enterExpr(rc)
      fields.foreach(traverseRecordField)
      listener.exitExpr(rc)
    case ru @ RecordUpdate(expr, _, fields) =>
      listener.enterExpr(ru)
      traverseExpr(expr)
      fields.foreach(traverseRecordField)
      listener.exitExpr(ru)
    case rs @ RecordSelect(expr, _, _) =>
      listener.enterExpr(rs)
      traverseExpr(expr)
      listener.exitExpr(rs)
    case ri @ RecordIndex(_, _) =>
      listener.enterExpr(ri)
      listener.exitExpr(ri)
    case mc @ MapCreate(kvs) =>
      listener.enterExpr(mc)
      kvs.foreach { kv =>
        traverseExpr(kv._1)
        traverseExpr(kv._2)
      }
      listener.exitExpr(mc)
    case gmu @ MapUpdate(map, kvs) =>
      listener.enterExpr(gmu)
      traverseExpr(map)
      kvs.foreach { kv =>
        traverseExpr(kv._1)
        traverseExpr(kv._2)
      }
      listener.exitExpr(gmu)
  }

  private def traversePat(pat: Pat): Unit = pat match {
    case pw @ PatWild() =>
      listener.enterPat(pw)
      listener.exitPat(pw)
    case pm @ PatMatch(pat, arg) =>
      listener.enterPat(pm)
      traversePat(pat)
      traversePat(arg)
      listener.exitPat(pm)
    case pt @ PatTuple(elems) =>
      listener.enterPat(pt)
      elems.foreach(traversePat)
      listener.exitPat(pt)
    case ps @ PatString() =>
      listener.enterPat(ps)
      listener.exitPat(ps)
    case pn @ PatNil() =>
      listener.enterPat(pn)
      listener.exitPat(pn)
    case pc @ PatCons(h, t) =>
      listener.enterPat(pc)
      traversePat(h)
      traversePat(t)
      listener.exitPat(pc)
    case pi @ PatInt() =>
      listener.enterPat(pi)
      listener.exitPat(pi)
    case pn @ PatNumber() =>
      listener.enterPat(pn)
      listener.exitPat(pn)
    case pa @ PatAtom(_) =>
      listener.enterPat(pa)
      listener.exitPat(pa)
    case pv @ PatVar(_) =>
      listener.enterPat(pv)
      listener.exitPat(pv)
    case pr @ PatRecord(_, fields, gen) =>
      listener.enterPat(pr)
      fields.foreach(f => traversePat(f.pat))
      gen.foreach(traversePat)
      listener.exitPat(pr)
    case pi @ PatRecordIndex(_, _) =>
      listener.enterPat(pi)
      listener.exitPat(pi)
    case uo @ PatUnOp(_, arg) =>
      listener.enterPat(uo)
      traversePat(arg)
      listener.exitPat(uo)
    case bo @ PatBinOp(_, arg1, arg2) =>
      listener.enterPat(bo)
      traversePat(arg1)
      traversePat(arg2)
      listener.exitPat(bo)
    case pb @ PatBinary(elems) =>
      listener.enterPat(pb)
      elems.foreach { elem =>
        traversePat(elem.pat)
        elem.size.foreach(traverseExpr)
      }
      listener.exitPat(pb)
    case pm @ PatMap(kvs) =>
      listener.enterPat(pm)
      kvs.foreach { kv =>
        traverseTest(kv._1)
        traversePat(kv._2)
      }
      listener.exitPat(pm)
  }

  private def traverseClause(clause: Clause): Unit = {
    listener.enterClause(clause)
    clause.pats.foreach(traversePat)
    clause.guards.foreach(traverseGuard)
    clause.body.exprs.foreach(traverseExpr)
    listener.exitClause(clause)
  }

  private def traverseGuard(guard: Guard): Unit = {
    listener.enterGuard(guard)
    guard.tests.foreach(traverseTest)
    listener.exitGuard(guard)
  }

  private def traverseTest(test: Test): Unit = test match {
    case tv @ TestVar(_) =>
      listener.enterTest(tv)
      listener.exitTest(tv)
    case ta @ TestAtom(_) =>
      listener.enterTest(ta)
      listener.exitTest(ta)
    case tn @ TestNumber(_) =>
      listener.enterTest(tn)
      listener.exitTest(tn)
    case tt @ TestTuple(elems) =>
      listener.enterTest(tt)
      elems.foreach(traverseTest)
      listener.exitTest(tt)
    case ts @ TestString() =>
      listener.enterTest(ts)
      listener.exitTest(ts)
    case tn @ TestNil() =>
      listener.enterTest(tn)
      listener.exitTest(tn)
    case tc @ TestCons(h, t) =>
      listener.enterTest(tc)
      traverseTest(h)
      traverseTest(t)
      listener.exitTest(tc)
    case tc @ TestCall(_, args) =>
      listener.enterTest(tc)
      args.foreach(traverseTest)
      listener.exitTest(tc)
    case tr @ TestRecordCreate(_, fields) =>
      listener.enterTest(tr)
      fields.foreach(f => traverseTest(f.value))
      listener.exitTest(tr)
    case tr @ TestRecordSelect(rec, _, _) =>
      listener.enterTest(tr)
      traverseTest(rec)
      listener.exitTest(tr)
    case tr @ TestRecordIndex(_, _) =>
      listener.enterTest(tr)
      listener.exitTest(tr)
    case tm @ TestMapCreate(kvs) =>
      listener.enterTest(tm)
      kvs.foreach { kv =>
        traverseTest(kv._1)
        traverseTest(kv._2)
      }
      listener.exitTest(tm)
    case tm @ TestMapUpdate(map, kvs) =>
      listener.enterTest(tm)
      traverseTest(map)
      kvs.foreach { kv =>
        traverseTest(kv._1)
        traverseTest(kv._2)
      }
      listener.exitTest(tm)
    case uo @ TestUnOp(_, arg) =>
      listener.enterTest(uo)
      traverseTest(arg)
      listener.exitTest(uo)
    case bo @ TestBinOp(_, arg1, arg2) =>
      listener.enterTest(bo)
      traverseTest(arg1)
      traverseTest(arg2)
      listener.exitTest(bo)
    case bl @ TestBinaryLit() =>
      listener.enterTest(bl)
      listener.exitTest(bl)
  }

  private def traverseQualifier(qualifier: Qualifier): Unit = qualifier match {
    case BGenerate(pat, expr) =>
      traversePat(pat)
      traverseExpr(expr)
    case LGenerate(pat, expr) =>
      traversePat(pat)
      traverseExpr(expr)
    case MGenerate(kPat, vPat, expr) =>
      traversePat(kPat)
      traversePat(vPat)
      traverseExpr(expr)
    case Filter(expr) =>
      traverseExpr(expr)
  }

  private def traverseBinaryElem(binaryElem: BinaryElem): Unit = {
    traverseExpr(binaryElem.expr)
    binaryElem.size.foreach(traverseExpr)
  }

  private def traverseRecordField(recordField: RecordField): Unit =
    traverseExpr(recordField.value)

  private def traverseTypeDecl(td: ExternalTypeDecl): Unit = {
    listener.enterTypeDecl(td)
    traverseType(td.body)
    listener.exitTypeDecl(td)
  }

  private def traverseOpaqueDecl(od: ExternalOpaqueDecl): Unit = {
    listener.enterOpaqueDecl(od)
    traverseType(od.body)
    listener.exitOpaqueDecl(od)
  }

  private def traverseType(tp: ExtType): Unit = tp match {
    case at @ AtomLitExtType(_) =>
      listener.enterType(at)
      listener.exitType(at)
    case ft @ FunExtType(argTys, resTy) =>
      listener.enterType(ft)
      argTys.foreach(traverseType)
      traverseType(resTy)
      listener.exitType(ft)
    case aft @ AnyArityFunExtType(resTy) =>
      listener.enterType(aft)
      traverseType(resTy)
      listener.exitType(aft)
    case tt @ TupleExtType(elemTys) =>
      listener.enterType(tt)
      elemTys.foreach(traverseType)
      listener.exitType(tt)
    case lt @ ListExtType(elemTy) =>
      listener.enterType(lt)
      traverseType(elemTy)
      listener.exitType(lt)
    case at @ AnyListExtType() =>
      listener.enterType(at)
      listener.exitType(at)
    case ut @ UnionExtType(tys) =>
      listener.enterType(ut)
      tys.foreach(traverseType)
      listener.exitType(ut)
    case lt @ LocalExtType(_, argTys) =>
      listener.enterType(lt)
      argTys.foreach(traverseType)
      listener.exitType(lt)
    case rt @ RemoteExtType(_, argTys) =>
      listener.enterType(rt)
      argTys.foreach(traverseType)
      listener.exitType(rt)
    case bt @ BuiltinExtType(_) =>
      listener.enterType(bt)
      listener.exitType(bt)
    case ilt @ IntLitExtType() =>
      listener.enterType(ilt)
      listener.exitType(ilt)
    case vt @ VarExtType(_) =>
      listener.enterType(vt)
      listener.exitType(vt)
    case rt @ RecordExtType(_) =>
      listener.enterType(rt)
      listener.exitType(rt)
    case mt @ MapExtType(propTys) =>
      listener.enterType(mt)
      propTys.foreach { protTy =>
        traverseType(protTy.key)
        traverseType(protTy.tp)
      }
      listener.exitType(mt)
    case at @ AnyMapExtType() =>
      listener.enterType(at)
      listener.exitType(at)
    case r @ RecordRefinedExtType(_, fields) =>
      listener.enterType(r)
      fields.foreach { f =>
        traverseType(f.ty)
      }
      listener.exitType(r)
    case unOp: UnOpType =>
      listener.enterType(unOp)
      listener.exitType(unOp)
    case binOp: BinOpType =>
      listener.enterType(binOp)
      listener.exitType(binOp)
  }

  private def traverseRecDecl(recDecl: ExternalRecDecl): Unit = {
    listener.enterRecDecl(recDecl)
    recDecl.fields.foreach(_.tp.foreach(traverseType))
    listener.exitRecDecl(recDecl)
  }

  private def traverseCompileExportAll(compileExportAll: CompileExportAll): Unit = {
    listener.enterCompileExportAll(compileExportAll)
    listener.exitCompileExportAll(compileExportAll)
  }

  private def traverseExport(e: Export): Unit = {
    listener.enterExport(e)
    listener.exitExport(e)
  }

  private def traverseFile(file: File): Unit = {
    listener.enterFile(file)
    listener.exitFile(file)
  }

  private def traverseTypingAttribute(typing: TypingAttribute): Unit = {
    listener.enterTypingAttribute(typing)
    listener.exitTypingAttribute(typing)
  }
}
