/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.BinarySpecifiers._
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.ExternalTypes._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.io.EData
import com.whatsapp.eqwalizer.io.EData._

class ConvertAst(fromBeam: Boolean, noAutoImport: Set[Id] = Set.empty) {

  private val specifiers: Map[String, Specifier] =
    Map(
      "default" -> UnsignedIntegerSpecifier,
      "integer" -> UnsignedIntegerSpecifier,
      "float" -> FloatSpecifier,
      "binary" -> BinarySpecifier,
      "bytes" -> BytesSpecifier,
      "bitstring" -> BitstringSpecifier,
      "bits" -> BitsSpecifier,
      "utf8" -> Utf8Specifier,
      "utf16" -> Utf16Specifier,
      "utf32" -> Utf32Specifier,
    )

  private def pos(_1: Int, _2: Int): Pos =
    if (fromBeam) LineAndColumn(_1, _2)
    else TextRange(_1, _2)

  object EPos {
    def unapply(term: EObject): Option[Pos] = term match {
      case ETuple(List(l: ELong, c: ELong)) =>
        Some(pos(l.value.intValue, c.value.intValue))
      case EList(List(ETuple(List(EAtom("text"), EString(_))), ETuple(List(EAtom("location"), pos))), None) =>
        unapply(pos)
      // We have some "bad code" after parse transforms.
      case l: ELong =>
        Some(pos(l.value.intValue, l.value.intValue + 1))
      case _ =>
        None
    }
  }
  object ELine {
    def unapply(term: EObject): Option[Int] = term match {
      case eLong: ELong =>
        Some(eLong.value.intValue)
      case _ =>
        None
    }
  }
  object EArity {
    def unapply(eLong: ELong): Option[Int] = Some(eLong.value.intValue)
  }

  def extractNoAutoImport(term: EObject): Option[List[Id]] =
    term match {
      case ETuple(
            List(EAtom("attribute"), EPos(_), EAtom("compile"), ETuple(List(EAtom("no_auto_import"), EList(ids, None))))
          ) =>
        Some(ids.map(convertIdInAttr))
      case _ =>
        None
    }

  def convertForm(term: EObject): Option[ExternalForm] =
    term match {
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("module"), EAtom(name))) =>
        Some(Module(name)(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("export"), EList(ids, None))) =>
        Some(Export(ids.map(convertIdInAttr))(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("import"), ETuple(List(EAtom(m), EList(ids, None))))) =>
        Some(Import(m, ids.map(convertIdInAttr))(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("export_type"), EList(typesIds, None))) =>
        Some(ExportType(typesIds.map(convertIdInAttr))(pos))
      case ETuple(
            List(EAtom("attribute"), EPos(pos), EAtom("record"), ETuple(List(EAtom(name), EList(fields, None))))
          ) =>
        Some(ExternalRecDecl(name, fields.map(recField))(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("file"), ETuple(List(EString(file), ELine(start))))) =>
        Some(File(file, start)(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("elp_metadata"), proplist: EList)) =>
        proplist.elems.collectFirst { case ETuple(List(EAtom("eqwalizer_fixmes"), EList(rawFixmes, _improper_tail))) =>
          val fixmes = rawFixmes.map {
            case ETuple(List(ELong(commentStart), ELong(commentEnd), ELong(suppressionStart), ELong(suppressionEnd))) =>
              val comment = TextRange(commentStart.toInt, commentEnd.toInt)
              val suppression = TextRange(suppressionStart.toInt, suppressionEnd.toInt)
              Fixme(comment, suppression)
            case _ =>
              throw new IllegalStateException()
          }
          ElpMetadata(fixmes)(pos)
        }
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("behaviour" | "behavior"), EAtom(name))) =>
        Some(Behaviour(name)(pos))
      case ETuple(List(EAtom("eof"), EPos(line))) =>
        None
      case ETuple(List(EAtom("attribute"), EPos(p), EAtom("type"), ETuple(List(EAtom(n), body, EList(vs, None))))) =>
        val id = Id(n, vs.size)
        Some(ExternalTypeDecl(id, vs.map(varString), convertType(body))(p))
      case ETuple(List(EAtom("attribute"), EPos(p), EAtom("opaque"), ETuple(List(EAtom(n), body, EList(vs, None))))) =>
        val id = Id(n, vs.size)
        Some(ExternalOpaqueDecl(id, vs.map(varString), convertType(body))(p))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("spec"), ETuple(List(eFunId, EList(eTypeList, None))))) =>
        Some(ExternalFunSpec(convertIdInAttr(eFunId), eTypeList.map(convertFunSpecType))(pos))
      case ETuple(
            List(EAtom("attribute"), EPos(pos), EAtom("callback"), ETuple(List(eFunId, EList(eTypeList, None))))
          ) =>
        Some(ExternalCallback(convertIdInAttr(eFunId), eTypeList.map(convertFunSpecType))(pos))
      case ETuple(List(EAtom("attribute"), EPos(pos), EAtom("optional_callbacks"), EList(funIds, None))) =>
        Some(ExternalOptionalCallbacks(funIds.map(convertIdInAttr))(pos))
      case ETuple(List(EAtom("attribute"), EPos(p), EAtom("compile"), attrValue)) if isExportAll(attrValue) =>
        Some(CompileExportAll()(p))
      case ETuple(List(EAtom("attribute"), EPos(p), EAtom("typing"), EList(elems, None)))
          if elems.forall(x => x.isInstanceOf[EAtom]) =>
        val names = elems.map(_.asInstanceOf[EAtom].atom)
        Some(TypingAttribute(names)(p))
      case ETuple(
            List(
              EAtom("attribute"),
              EPos(p),
              EAtom("eqwalizer"),
              ETuple(List(EAtom("nowarn_function"), ETuple(List(EAtom(name), ELong(arity))))),
            )
          ) =>
        Some(EqwalizerNowarnFunction(Id(name, arity.toInt))(p))
      case ETuple(
            List(
              EAtom("attribute"),
              EPos(p),
              EAtom("eqwalizer"),
              ETuple(List(EAtom("unlimited_refinement"), ETuple(List(EAtom(name), ELong(arity))))),
            )
          ) =>
        Some(EqwalizerUnlimitedRefinement(Id(name, arity.toInt))(p))
      case ETuple(EAtom("attribute") :: _) =>
        None
      case ETuple(List(EAtom("function"), EPos(pos), EAtom(name), EArity(arity), EList(clauseSeq, None))) =>
        val funId = Id(name, arity)
        Some(FunDecl(funId, clauseSeq.map(convertClause))(pos))
      case _ => throw new IllegalStateException(term.toString)
    }

  private def recField(term: EObject): ExternalRecField =
    term match {
      case ETuple(List(EAtom("record_field"), EPos(p), fieldNameLit)) =>
        ExternalRecField(atomLit(fieldNameLit), None, None)
      case ETuple(List(EAtom("record_field"), EPos(p), fieldNameLit, expr)) =>
        val defaultValue = convertExp(expr)
        ExternalRecField(atomLit(fieldNameLit), None, Some(defaultValue))
      case ETuple(List(EAtom("typed_record_field"), eUntypedField, eType)) =>
        val (name, defaultValue, i) =
          eUntypedField match {
            case ETuple(List(EAtom("record_field"), EPos(p), fieldNameLit)) =>
              (atomLit(fieldNameLit), None, p)
            case ETuple(List(EAtom("record_field"), EPos(p), fieldNameLit, expr)) =>
              val defaultValue = convertExp(expr)
              (atomLit(fieldNameLit), Some(defaultValue), p)
            case _ => throw new IllegalStateException()
          }
        val tp = convertType(eType)
        ExternalRecField(name, Some(tp), defaultValue)
      case _ => throw new IllegalStateException()
    }

  private def varString(term: EObject): String = {
    val ETuple(List(EAtom("var"), _, EAtom(name))) = term
    name
  }

  private def convertIdInAttr(term: EObject): Id =
    term match {
      case ETuple(List(EAtom(name), EArity(arity))) =>
        Id(name, arity)
      case ETuple(List(EAtom(module), EAtom(name), EArity(arity))) =>
        // it should be the same module by construction, -> localizing
        Id(name, arity)
      case _ => throw new IllegalStateException()
    }

  private def atomLit(term: EObject): String = {
    val ETuple(List(EAtom("atom"), _, EAtom(atomVal))) = term
    atomVal
  }

  private def recFieldName(term: EObject): Option[String] = term match {
    case ETuple(List(EAtom("atom"), _, EAtom(atomVal))) =>
      Some(atomVal)
    case ETuple(List(EAtom("var"), EPos(p), EAtom("_"))) =>
      None
    case _ => throw new IllegalStateException()
  }

  private def convertType(term: EObject): ExtType =
    term match {
      case ETuple(List(EAtom("ann_type"), _, EList(List(af_anno, tp), None))) =>
        convertType(tp)
      case ETuple(List(EAtom("atom"), EPos(pos), EAtom(atomVal))) =>
        AtomLitExtType(atomVal)(pos)
      case ETuple(
            List(
              EAtom("type"),
              EPos(pos),
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("any"))), resType), _),
            )
          ) =>
        AnyArityFunExtType(convertType(resType))(pos)
      case ETuple(
            List(
              EAtom("type"),
              EPos(pos),
              EAtom("fun"),
              EList(List(ETuple(List(EAtom("type"), _, EAtom("product"), EList(args, None))), resultType), None),
            )
          ) =>
        FunExtType(args.map(convertType), convertType(resultType))(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("range"), EList(List(rangeFirst, rangeLast), _))) =>
        (convertRangePart(rangeFirst), convertRangePart(rangeLast)) match {
          case (Some(first), Some(last)) =>
            RangeExtType(first, last)(pos)
          case _ =>
            intExtType(pos)
        }
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("map"), EAtom("any"))) =>
        AnyMapExtType()(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("map"), EList(assocTypes, _))) =>
        if (assocTypes.isEmpty) {
          MapExtType(List.empty)(pos)
        } else if (assocTypes.length == 1)
          assocTypes.head match {
            case ETuple(List(EAtom("type"), EPos(propLine), EAtom("map_field_assoc"), EList(List(ekT, evT), None))) =>
              val keyType = convertType(ekT)
              val valType = convertType(evT)
              MapExtType(List(OptExtProp(keyType, valType)(propLine)))(pos)
            case prop =>
              MapExtType(List(convertPropType(prop)))(pos)
          }
        else
          MapExtType(assocTypes.map(convertPropType))(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("record"), EList(recordName :: eFieldTypes, None))) =>
        if (eFieldTypes.isEmpty)
          RecordExtType(atomLit(recordName))(pos)
        else
          RecordRefinedExtType(atomLit(recordName), eFieldTypes.map(convertRefinedField))(pos)
      case ETuple(
            List(EAtom("remote_type"), EPos(pos), EList(List(moduleLit, typeNameLit, EList(args, None)), None))
          ) =>
        RemoteExtType(
          RemoteId(atomLit(moduleLit), atomLit(typeNameLit), args.size),
          args.map(convertType),
        )(pos)
      case ETuple(List(EAtom("user_type"), EPos(pos), EAtom(name), EList(params, None))) =>
        LocalExtType(Id(name, params.size), params.map(convertType))(pos)
      case ETuple(List(EAtom("integer"), EPos(pos), ELong(value))) =>
        IntLitExtType(value)(pos)
      case ETuple(List(EAtom("char"), EPos(pos), ELong(_))) =>
        BuiltinExtType("char")(pos)
      case ETuple(List(EAtom("op"), EPos(pos), EAtom(op), _)) =>
        UnOpType(op)(pos)
      case ETuple(List(EAtom("op"), EPos(pos), EAtom(op), _, _)) =>
        BinOpType(op)(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("tuple"), EAtom("any"))) =>
        BuiltinExtType("tuple")(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("tuple"), EList(types, None))) =>
        TupleExtType(types.map(convertType))(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("union"), EList(types, None))) =>
        UnionExtType(types.map(convertType))(pos)
      case ETuple(List(EAtom("var"), EPos(pos), EAtom("_"))) =>
        anyExtType(pos)
      case ETuple(List(EAtom("var"), EPos(pos), EAtom(name))) =>
        VarExtType(name)(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("list" | "nonempty_list"), EList(List(elemType), None))) =>
        ListExtType(convertType(elemType))(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("list" | "nonempty_list"), EList(List(), None))) =>
        AnyListExtType()(pos)
      case ETuple(
            List(
              EAtom("type"),
              EPos(pos),
              EAtom("maybe_improper_list" | "nonempty_improper_list" | "nonempty_maybe_improper_list"),
              EList(List(elemType, _), None),
            )
          ) =>
        ListExtType(convertType(elemType))(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom(name), EList(List(), None))) =>
        assert(Types.builtinTypes.contains(name), s"unknown builtin type $name/0")
        BuiltinExtType(name)(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom("binary"), EList(params, None))) =>
        assert(params.size == 1 || params.size == 2)
        BuiltinExtType("binary")(pos)
      case ETuple(List(EAtom("type"), EPos(pos), EAtom(name), EList(params, None))) =>
        val arity = params.size
        throw new IllegalStateException(s"unknown builtin type $name/$arity")
      case _ => throw new IllegalStateException()
    }

  private def convertPropType(t: EObject): ExtProp =
    t match {
      case ETuple(List(EAtom("type"), EPos(p), EAtom("map_field_assoc"), EList(List(ekT, evT), None))) =>
        val keyType = convertType(ekT)
        val valType = convertType(evT)
        keyType match {
          case AtomLitExtType(k) =>
            OptExtProp(keyType, valType)(p)
          case _ =>
            OptBadExtProp(keyType, valType)(p)
        }
      case ETuple(List(EAtom("type"), EPos(p), EAtom("map_field_exact"), EList(List(ekT, evT), None))) =>
        val keyType = convertType(ekT)
        val valType = convertType(evT)
        keyType match {
          case AtomLitExtType(_) =>
            ReqExtProp(keyType, valType)(p)
          case _ =>
            ReqBadExtProp(keyType, valType)(p)
        }
      case _ => throw new IllegalStateException()
    }

  private def convertFunSpecType(term: EObject): ConstrainedFunType =
    term match {
      case ETuple(
            List(
              EAtom("type"),
              _,
              EAtom("fun"),
              EList(
                List(ETuple(List(EAtom("type"), EPos(pos), EAtom("product"), EList(args, None))), resultType),
                None,
              ),
            )
          ) =>
        ConstrainedFunType(FunExtType(args.map(convertType), convertType(resultType))(pos), Nil)(pos)
      case ETuple(
            List(EAtom("type"), EPos(pos), EAtom("bounded_fun"), EList(List(ft, EList(constraints, None)), None))
          ) =>
        ConstrainedFunType(convertType(ft).asInstanceOf[FunExtType], constraints.map(convertConstraint))(pos)
      case _ => throw new IllegalStateException()
    }

  private def convertConstraint(term: EObject): Constraint = {
    val ETuple(
      List(EAtom("type"), EPos(pos), EAtom("constraint"), EList(List(isSub, EList(List(v, t), None)), None))
    ) = term
    val "is_subtype" = atomLit(isSub)
    Constraint(varString(v), convertType(t))(pos)
  }

  private def convertClause(term: EObject): Clause = {
    val ETuple(List(EAtom("clause"), EPos(p), EList(ePats, None), EList(eGuards, None), EList(eExps, None))) = term
    Clause(ePats.map(convertPat), eGuards.map(convertGuard), Body(eExps.map(convertExp)))(p)
  }

  private def convertExp(term: EObject): Expr =
    term match {
      case ETuple(List(EAtom("match"), EPos(p), ePat1, eExp)) =>
        Match(convertPat(ePat1), convertExp(eExp))(p)
      case ETuple(List(EAtom("var"), EPos(p), EAtom(name))) =>
        Var(name)(p)
      case ETuple(List(EAtom("tuple"), EPos(p), EList(eExps, None))) =>
        Tuple(eExps.map(convertExp))(p)
      case ETuple(List(EAtom("nil"), EPos(p))) =>
        NilLit()(p)
      case ETuple(List(EAtom("cons"), EPos(p), hE, tE)) =>
        Cons(convertExp(hE), convertExp(tE))(p)
      case ETuple(List(EAtom("bin"), EPos(p), EList(binElems, None))) =>
        Binary(binElems.map(convertBinaryElem))(p)
      case ETuple(List(EAtom("op"), EPos(p), EAtom(op), exp1, exp2)) =>
        BinOp(op, convertExp(exp1), convertExp(exp2))(p)
      case ETuple(List(EAtom("op"), EPos(p), EAtom(op), exp)) =>
        UnOp(op, convertExp(exp))(p)
      case ETuple(List(EAtom("record"), EPos(p), EAtom(recordName), EList(eRecordFieldExps, None))) =>
        RecordCreate(recordName, eRecordFieldExps.map(convertRecordField))(p)
      case ETuple(List(EAtom("record"), EPos(p), eExp, EAtom(recordName), EList(eRecordFieldExps, None))) =>
        // Erlang doesn't allow "fruitless _ updates"
        val namedFields = eRecordFieldExps.map(e => convertRecordField(e).asInstanceOf[RecordFieldNamed])
        RecordUpdate(convertExp(eExp), recordName, namedFields)(p)
      case ETuple(List(EAtom("record_index"), EPos(p), EAtom(recordName), eFieldName)) =>
        RecordIndex(recordName, atomLit(eFieldName))(p)
      case ETuple(List(EAtom("record_field"), EPos(p), eExp, EAtom(recordName), eFieldName)) =>
        RecordSelect(convertExp(eExp), recordName, atomLit(eFieldName))(p)
      case ETuple(List(EAtom("map"), EPos(p), EList(eAssocs, None))) =>
        MapCreate(eAssocs.map(convertCreateKV))(p)
      case ETuple(List(EAtom("map"), EPos(p), eExp, EList(eAssocs, None))) =>
        val map = convertExp(eExp)
        if (eAssocs.isEmpty)
          GenMapUpdate(map, List())(p, false)
        else if (eAssocs.forall(isMandatoryAtomicField))
          ReqMapUpdate(map, eAssocs.map(convertAV))(p)
        else {
          val approximated = eAssocs.exists(isMandatoryField)
          GenMapUpdate(map, eAssocs.map(convertKV))(p, approximated)
        }
      case ETuple(List(EAtom("catch"), EPos(p), eExp)) =>
        Catch(convertExp(eExp))(p)
      case ETuple(List(EAtom("call"), EPos(p), eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), EPos(_), EAtom(m))),
                  ETuple(List(EAtom("atom"), EPos(_), EAtom(f))),
                )
              ) =>
            RemoteCall(RemoteId(m, f, eArgs.size), eArgs.map(convertExp))(p)
          case ETuple(List(EAtom("atom"), EPos(_), EAtom(fname))) =>
            val localId = Id(fname, eArgs.size)
            if (CompilerMacro.funs(localId)) {
              val remoteId = RemoteId(CompilerMacro.fake_module, fname, eArgs.size)
              RemoteCall(remoteId, eArgs.map(convertExp))(p)
            } else if (AutoImport.funs(localId) && !noAutoImport(localId)) {
              val remoteId = RemoteId("erlang", fname, eArgs.size)
              RemoteCall(remoteId, eArgs.map(convertExp))(p)
            } else {
              LocalCall(localId, eArgs.map(convertExp))(p)
            }
          case _ =>
            DynCall(convertExp(eExp), eArgs.map(convertExp))(p)
        }
      case ETuple(List(EAtom("lc"), EPos(p), eTemplate, EList(eQualifiers, None))) =>
        LComprehension(convertExp(eTemplate), eQualifiers.map(convertQualifier))(p)
      case ETuple(List(EAtom("bc"), EPos(p), eTemplate, EList(eQualifiers, None))) =>
        BComprehension(convertExp(eTemplate), eQualifiers.map(convertQualifier))(p)
      case ETuple(List(EAtom("block"), EPos(p), EList(eExps, None))) =>
        Block(Body(eExps.map(convertExp)))(p)
      case ETuple(List(EAtom("if"), EPos(p), EList(eClauses, None))) =>
        If(eClauses.map(convertClause))(p)
      case ETuple(List(EAtom("case"), EPos(p), eExp, EList(eClauses, None))) =>
        Case(convertExp(eExp), eClauses.map(convertClause))(p)
      case ETuple(
            List(
              EAtom("try"),
              EPos(p),
              EList(eTryBody, None),
              EList(eTryClauses, None),
              EList(eCatchClauses, None),
              EList(eAfter, None),
            )
          ) =>
        val tryBody = Body(eTryBody.map(convertExp))
        val tryClauses = eTryClauses.map(convertClause)
        val catchClauses = eCatchClauses.map(convertClause)
        val afterBody = if (eAfter.isEmpty) None else Some(Body(eAfter.map(convertExp)))
        if (tryClauses.isEmpty)
          TryCatchExpr(tryBody, catchClauses, afterBody)(p)
        else
          TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody)(p)
      case ETuple(List(EAtom("receive"), EPos(p), EList(eClauses, None))) =>
        Receive(eClauses.map(convertClause))(p)
      case ETuple(List(EAtom("receive"), EPos(p), EList(eClauses, None), eTimeout, EList(defaults, None))) =>
        ReceiveWithTimeout(eClauses.map(convertClause), convertExp(eTimeout), Body(defaults.map(convertExp)))(p)
      case ETuple(List(EAtom("fun"), EPos(p), eFunction)) =>
        eFunction match {
          case ETuple(List(EAtom("clauses"), EList(eClauses, None))) =>
            Lambda(eClauses.map(convertClause))(p, name = None)
          case ETuple(List(EAtom("function"), EAtom(name), EArity(arity))) =>
            val localId = Id(name, arity)
            if (AutoImport.funs(localId) && !noAutoImport(localId)) {
              val remoteId = RemoteId("erlang", name, arity)
              RemoteFun(remoteId)(p)
            } else {
              LocalFun(Id(name, arity))(p)
            }
          case ETuple(
                List(
                  EAtom("function"),
                  ETuple(List(EAtom("atom"), _, EAtom(module))),
                  ETuple(List(EAtom("atom"), _, EAtom(name))),
                  ETuple(List(EAtom("integer"), _, EArity(arity))),
                )
              ) =>
            RemoteFun(RemoteId(module, name, arity))(p)
          case ETuple(List(EAtom("function"), mod, name, arity)) =>
            DynRemoteFunArity(convertExp(mod), convertExp(name), convertExp(arity))(p)
          case _ => throw new IllegalStateException()
        }
      case ETuple(List(EAtom("named_fun"), EPos(p), EAtom(name), EList(eClauses, None))) =>
        Lambda(eClauses.map(convertClause))(p, Some(name))
      case ETuple(List(EAtom("atom"), EPos(p), EAtom(value))) =>
        AtomLit(value)(p)
      case ETuple(List(EAtom("float"), EPos(p), EDouble(_))) =>
        FloatLit()(p)
      case ETuple(List(EAtom("char" | "integer"), EPos(p), ELong(value))) =>
        IntLit(value)(p)
      case ETuple(List(EAtom("string"), EPos(p), stringValue)) =>
        val emptyString = stringValue match {
          case EList(Nil, None) => true
          case _                => false
        }
        StringLit(emptyString)(p)
      case ETuple(List(EAtom("remote"), EPos(p), mod, name)) =>
        DynRemoteFun(convertExp(mod), convertExp(name))(p)
      case _ => throw new IllegalStateException()
    }

  private def convertCreateKV(term: EObject): (Expr, Expr) = {
    val ETuple(List(EAtom("map_field_assoc"), _, eExp1, eExp2)) = term
    (convertExp(eExp1), convertExp(eExp2))
  }

  private def convertKV(term: EObject): (Expr, Expr) = {
    val ETuple(List(_, _, eExp1, eExp2)) = term
    (convertExp(eExp1), convertExp(eExp2))
  }

  private def convertAV(term: EObject): (String, Expr) = {
    val ETuple(List(EAtom("map_field_exact"), EPos(p), ETuple(List(EAtom("atom"), _, EAtom(key))), eExp2)) = term
    (key, convertExp(eExp2))
  }

  private def isMandatoryField(term: EObject): Boolean =
    term match {
      case ETuple(List(EAtom("map_field_exact"), _, _, _)) =>
        true
      case _ =>
        false
    }

  private def isMandatoryAtomicField(term: EObject): Boolean =
    term match {
      case ETuple(List(EAtom("map_field_exact"), _, ETuple(List(EAtom("atom"), _, _)), _)) =>
        true
      case _ =>
        false
    }

  private def convertRecordField(term: EObject): RecordField =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, exp)) =>
        recFieldName(eName) match {
          case Some(name) => RecordFieldNamed(name, convertExp(exp))
          case None       => RecordFieldGen(convertExp(exp))
        }

      case _ => throw new IllegalStateException()
    }

  private def convertRefinedField(term: EObject): RefinedField =
    term match {
      case ETuple(List(EAtom("type"), _anno, EAtom("field_type"), EList(List(nameLit, eType), None))) =>
        RefinedField(atomLit(nameLit), convertType(eType))
      case _ => throw new IllegalStateException()
    }

  private def convertBinaryElem(e: EObject): BinaryElem = {
    val ETuple(List(EAtom("bin_element"), EPos(p), elem, eSize, eSpecifier)) = e
    val size = eSize match {
      case EAtom("default") => None
      case _                => Some(convertExp(eSize))
    }
    val specifier = convertSpecifier(eSpecifier)
    BinaryElem(convertExp(elem), size, specifier)(p)
  }

  private def convertGuard(term: EObject): Guard = {
    val EList(tests, None) = term
    Guard(tests.map(convertTest))
  }

  private def convertPat(term: EObject): Pat =
    term match {
      case ETuple(List(EAtom("match"), EPos(p), ePat1, ePat2)) =>
        PatMatch(convertPat(ePat1), convertPat(ePat2))(p)
      case ETuple(List(EAtom("var"), EPos(p), EAtom("_"))) =>
        PatWild()(p)
      case ETuple(List(EAtom("var"), EPos(p), EAtom(name))) =>
        PatVar(name)(p)
      case ETuple(List(EAtom("tuple"), EPos(p), EList(ePats, None))) =>
        PatTuple(ePats.map(convertPat))(p)
      case ETuple(List(EAtom("nil"), EPos(p))) =>
        PatNil()(p)
      case ETuple(List(EAtom("cons"), EPos(p), hPat, tPat)) =>
        PatCons(convertPat(hPat), convertPat(tPat))(p)
      case ETuple(List(EAtom("atom"), EPos(p), EAtom(value))) =>
        PatAtom(value)(p)
      case ETuple(List(EAtom("float"), EPos(p), _)) =>
        PatNumber()(p)
      case ETuple(List(EAtom("char" | "integer"), EPos(p), ELong(value))) =>
        PatInt()(p)
      case ETuple(List(EAtom("string"), EPos(p), _)) =>
        PatString()(p)
      case ETuple(List(EAtom("bin"), EPos(p), EList(binElems, None))) =>
        PatBinary(binElems.map(convertPatBinaryElem))(p)
      case ETuple(List(EAtom("op"), EPos(p), EAtom(op), p1, p2)) =>
        PatBinOp(op, convertPat(p1), convertPat(p2))(p)
      case ETuple(List(EAtom("op"), EPos(pos), EAtom(op), p)) =>
        PatUnOp(op, convertPat(p))(pos)
      case ETuple(List(EAtom("record"), EPos(p), EAtom(name), EList(eFields, None))) =>
        PatRecord(name, eFields.flatMap(convertPatRecordField), eFields.flatMap(convertPatRecordFieldGen).headOption)(p)
      case ETuple(List(EAtom("record_index"), EPos(p), EAtom(name), eFieldName)) =>
        PatRecordIndex(name, atomLit(eFieldName))(p)
      case ETuple(List(EAtom("map"), EPos(p), EList(kvs, None))) =>
        PatMap(kvs.map(convertPatKV))(p)
      case _ => throw new IllegalStateException()
    }

  private def convertPatKV(term: EObject): (Test, Pat) = {
    val ETuple(List(EAtom("map_field_exact" | "map_field_assoc"), _, test, pat)) = term
    (convertTest(test), convertPat(pat))
  }

  private def convertPatRecordField(term: EObject): Option[PatRecordFieldNamed] = {
    val ETuple(List(EAtom("record_field"), _, eName, ePat)) = term
    recFieldName(eName).map(PatRecordFieldNamed(_, convertPat(ePat)))
  }

  private def convertPatRecordFieldGen(term: EObject): Option[Pat] = {
    val ETuple(List(EAtom("record_field"), _, eName, ePat)) = term
    recFieldName(eName) match {
      case None => Some(convertPat(ePat))
      case _    => None
    }
  }

  private def convertPatBinaryElem(e: EObject): PatBinaryElem = {
    val ETuple(List(EAtom("bin_element"), EPos(p), elem, eSize, eSpecifier)) = e
    val size = eSize match {
      case EAtom("default") =>
        None
      case _ =>
        Some(convertExp(eSize))
    }
    val specifier = convertSpecifier(eSpecifier)
    PatBinaryElem(convertPat(elem), size, specifier)(p)
  }

  private def convertQualifier(term: EObject): Qualifier =
    term match {
      case ETuple(List(EAtom("generate"), EPos(p), ePat, eExp)) =>
        LGenerate(convertPat(ePat), convertExp(eExp))
      case ETuple(List(EAtom("b_generate"), EPos(p), ePat, eExp)) =>
        BGenerate(convertPat(ePat), convertExp(eExp))
      case _ =>
        Filter(convertExp(term))
    }

  private def convertTest(term: EObject): Test =
    term match {
      case ETuple(List(EAtom("var"), EPos(p), EAtom(name))) =>
        TestVar(name)(p)
      case ETuple(List(EAtom("tuple"), EPos(p), EList(eTests, None))) =>
        TestTuple(eTests.map(convertTest))(p)
      case ETuple(List(EAtom("nil"), EPos(p))) =>
        TestNil()(p)
      case ETuple(List(EAtom("cons"), EPos(p), h, t)) =>
        TestCons(convertTest(h), convertTest(t))(p)
      case ETuple(List(EAtom("bin"), EPos(p), _)) =>
        TestBinaryLit()(p)
      case ETuple(List(EAtom("op"), EPos(p), EAtom(op), t1, t2)) =>
        TestBinOp(op, convertTest(t1), convertTest(t2))(p)
      case ETuple(List(EAtom("op"), EPos(p), EAtom(op), t)) =>
        TestUnOp(op, convertTest(t))(p)
      case ETuple(List(EAtom("record"), EPos(p), EAtom(recName), EList(eFields, None))) =>
        TestRecordCreate(recName, eFields.map(convertTestRecordField))(p)
      case ETuple(List(EAtom("record_index"), EPos(p), EAtom(recName), eFieldName)) =>
        TestRecordIndex(recName, atomLit(eFieldName))(p)
      case ETuple(List(EAtom("record_field"), EPos(p), eTest, EAtom(recName), eFieldName)) =>
        TestRecordSelect(convertTest(eTest), recName, atomLit(eFieldName))(p)
      case ETuple(List(EAtom("map"), EPos(p), EList(kvs, None))) =>
        TestMapCreate(kvs.map(convertTestKV))(p)
      case ETuple(List(EAtom("map"), EPos(p), t, EList(kvs, None))) =>
        val map = convertTest(t)
        TestMapUpdate(map, kvs.map(convertTestKV))(p)
      case ETuple(List(EAtom("call"), EPos(p), eExp, EList(eArgs, None))) =>
        eExp match {
          case ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), EPos(_), EAtom("erlang"))),
                  ETuple(List(EAtom("atom"), EPos(_), EAtom(fname))),
                )
              ) =>
            TestCall(Id(fname, eArgs.size), eArgs.map(convertTest))(p)
          case ETuple(List(EAtom("atom"), EPos(_), EAtom(fname))) =>
            TestCall(Id(fname, eArgs.size), eArgs.map(convertTest))(p)
          case _ => throw new IllegalStateException()
        }
      case ETuple(List(EAtom("atom"), EPos(p), EAtom(value))) =>
        TestAtom(value)(p)
      case ETuple(List(EAtom("float"), EPos(p), _value)) =>
        TestNumber(None)(p)
      case ETuple(List(EAtom("char" | "integer"), EPos(p), ELong(value))) =>
        TestNumber(Some(value))(p)
      case ETuple(List(EAtom("string"), EPos(p), _value)) =>
        TestString()(p)
      case _ => throw new IllegalStateException()
    }

  private def convertTestKV(term: EObject): (Test, Test) = {
    val ETuple(List(_assocType, _pos, t1, t2)) = term
    (convertTest(t1), convertTest(t2))
  }

  private def convertTestRecordField(term: EObject): TestRecordField = {
    val ETuple(List(EAtom("record_field"), _, eName, eVal)) = term
    recFieldName(eName) match {
      case Some(fieldName) => TestRecordFieldNamed(fieldName, convertTest(eVal))
      case None            => TestRecordFieldGen(convertTest(eVal))
    }
  }

  private def convertSpecifier(eSpecifier: EObject): Specifier = {
    val unsignedSpec = eSpecifier match {
      case EList(specs, None) =>
        specs.collect { case EAtom(s) => s }.flatMap(specifiers.get).headOption.getOrElse(UnsignedIntegerSpecifier)
      case _ =>
        UnsignedIntegerSpecifier
    }
    val signed = eSpecifier match {
      case EList(specs, None) => specs.contains(EAtom("signed"))
      case _                  => false
    }

    val spec =
      if (signed && unsignedSpec == UnsignedIntegerSpecifier)
        SignedIntegerSpecifier
      else unsignedSpec
    spec
  }

  private class ExportAllExtractor extends EData.Visitor {
    var exportAll: Boolean = false
    override def visit(obj: EObject): Unit = obj match {
      case EAtom("export_all") =>
        exportAll = true
      case _ =>
        ()
    }
  }

  private def isExportAll(obj: EObject): Boolean = {
    val extractor = new ExportAllExtractor
    EData.traverse(obj, extractor)
    extractor.exportAll
  }

  private def convertRangePart(r: EObject): Option[BigInt] = r match {
    case ETuple(List(EAtom("integer"), _, ELong(value))) =>
      Some(value)
    case ETuple(
          List(
            EAtom("op"),
            ETuple(_),
            EAtom(op),
            ETuple(List(EAtom("integer"), _pos, ELong(value))),
          )
        ) =>
      op match {
        case "-" =>
          Some(0 - value)
        case "+" =>
          Some(value)
        case _ =>
          throw new IllegalStateException(s"unexpected op $op")
      }
    case _tooComplicated =>
      None
  }
}
