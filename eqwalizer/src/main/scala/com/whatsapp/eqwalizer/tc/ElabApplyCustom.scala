/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec
import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Cons, Expr, Lambda, NilLit}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{Exprs, Pos, RemoteId}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{ExpectedSubtype, UnboundVar}

class ElabApplyCustom(pipelineContext: PipelineContext) {
  private lazy val elab = pipelineContext.elab
  private lazy val elabApply = pipelineContext.elabApply
  private lazy val check = pipelineContext.check
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val occurrence = pipelineCtx.occurrence
  implicit val pipelineCtx = pipelineContext

  def isCustom(remoteId: RemoteId): Boolean =
    custom(remoteId)

  private lazy val custom: Set[RemoteId] =
    Set(
      RemoteId("file", "open", 2),
      RemoteId("lists", "filtermap", 2),
      RemoteId("lists", "flatten", 1),
      RemoteId("lists", "flatten", 2),
      RemoteId("lists", "keysort", 2),
      RemoteId("lists", "keystore", 4),
      RemoteId("maps", "filter", 2),
      RemoteId("maps", "find", 2),
      RemoteId("maps", "fold", 3),
      RemoteId("maps", "get", 2),
      RemoteId("maps", "get", 3),
      RemoteId("maps", "map", 2),
      RemoteId("maps", "remove", 2),
      RemoteId("maps", "put", 3),
      RemoteId("maps", "with", 2),
      RemoteId("maps", "without", 2),
    )

  def elabCustom(remoteId: RemoteId, args: List[Expr], env: Env, callPos: Pos): (Type, Env) = {
    val (argTys, env1) = elab.elabExprs(args, env)
    remoteId match {
      case fqn @ RemoteId("file", "open", 2) =>
        util.getFunType(fqn) match {
          case Some(ft) =>
            val resTy = elabApply.elabApply(check.freshen(ft), args, argTys, env1)
            val resTyPrecise = {
              val modeArg = args(1)
              def collectModes(expr: Expr): List[Option[String]] = {
                expr match {
                  case NilLit() =>
                    Nil
                  case Cons(AtomLit(s), t) =>
                    Some(s) :: collectModes(t)
                  case Cons(Exprs.Tuple(List(AtomLit(s), _)), t) =>
                    Some(s) :: collectModes(t)
                  case _ =>
                    List(None)
                }
              }
              val modes = collectModes(modeArg)
              if (modes.forall(_.isDefined)) {
                val literalModes = modes.flatten
                val fd = literalModes.contains("raw") || literalModes.contains("ram")
                val deviceT =
                  if (fd)
                    RemoteType(RemoteId("file", "fd", 0), List())
                  else
                    PidType
                UnionType(
                  Set(
                    TupleType(List(AtomLitType("ok"), deviceT)),
                    TupleType(
                      List(
                        AtomLitType("error"),
                        UnionType(
                          Set(
                            RemoteType(RemoteId("file", "posix", 0), List()),
                            AtomLitType("badarg"),
                            AtomLitType("system_limit"),
                          )
                        ),
                      )
                    ),
                  )
                )
              } else
                resTy
            }
            (resTyPrecise, env1)
          case None =>
            throw UnboundVar(callPos, fqn.toString)
        }
      case RemoteId("lists", "filtermap", 2) =>
        val List(funArg, collection) = args
        val List(funArgTy, collectionTy) = argTys
        val listAnyTy = ListType(AnyType)
        if (!subtype.subType(collectionTy, listAnyTy))
          throw ExpectedSubtype(collection.pos, collection, listAnyTy, collectionTy)
        val elemTy = narrow.asListType(collectionTy).get.t
        val tupleTrueAnyTy = TupleType(List(trueType, AnyType))
        val expRet = subtype.join(booleanType, tupleTrueAnyTy)
        val expFunTy = FunType(Nil, List(elemTy), expRet)
        val funResTys = funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, expFunTy, env1)
            if (occurrence.eqwater(lambda.clauses)) {
              val clauseEnvs = occurrence.clausesEnvs(lambda.clauses, List(elemTy), env1)
              lambda.clauses
                .lazyZip(clauseEnvs)
                .map((clause, occEnv) => elab.elabClause(clause, List(elemTy), occEnv, Set.empty)._1)
            } else {
              lambda.clauses.map(elab.elabClause(_, List(elemTy), env, Set.empty)).map(_._1)
            }
          case _ =>
            if (!subtype.subType(funArgTy, expFunTy))
              throw ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy)
            narrow.asFunType(funArgTy, 1).get.map(_.resTy)
        }
        def funResultsToItemTy(tys: Iterable[Type], defaultTy: Type, pos: Pos): Type =
          tys.foldLeft(NoneType: Type)((memo, ty) => subtype.join(memo, funResultToItemTy(ty, defaultTy, pos)))
        def funResultToItemTy(ty: Type, defaultTy: Type, pos: Pos): Type = ty match {
          case UnionType(tys) =>
            funResultsToItemTy(tys, defaultTy, pos)
          case TupleType(List(t1, resElemTy)) if subtype.subType(t1, trueType) =>
            resElemTy
          case ty if subtype.subType(ty, falseType) =>
            NoneType
          case ty if subtype.subType(ty, booleanType) =>
            defaultTy
          case _ =>
            // $COVERAGE-OFF$
            throw new IllegalArgumentException(s"unexpected $ty")
          // $COVERAGE-ON$
        }
        val resItemTy = funResultsToItemTy(funResTys, elemTy, callPos)
        (ListType(resItemTy), env1)

      case fqn @ RemoteId("lists", "flatten", arity @ (1 | 2)) =>
        val arg = args.head
        val argTy = argTys.head
        val anyListTy = ListType(AnyType)
        if (!subtype.subType(argTy, anyListTy))
          throw ExpectedSubtype(arg.pos, arg, expected = anyListTy, got = argTy)
        def flattenElemTy(ty: Type, seenAliases: Set[RemoteType] = Set()): Type = ty match {
          case ListType(ty)   => flattenElemTy(ty, seenAliases)
          case NilType        => NoneType
          case UnionType(tys) => subtype.join(tys.map(ty => flattenElemTy(ty, seenAliases)))
          case rt @ RemoteType(rid, args) =>
            if (seenAliases.contains(rt))
              NoneType
            else
              flattenElemTy(util.getTypeDeclBody(rid, args), seenAliases + rt)
          case ty => ty
        }
        val argElemTy = flattenElemTy(argTy)
        val elemTy = arity match {
          case 1 => argElemTy
          case 2 =>
            val tail = args(1)
            val tailTy = argTys(1)
            if (!subtype.subType(tailTy, anyListTy))
              throw ExpectedSubtype(tail.pos, tail, expected = anyListTy, got = tailTy)
            val Some(ListType(tailElemTy)) = narrow.asListType(tailTy)
            subtype.join(argElemTy, tailElemTy)
        }
        (ListType(elemTy), env1)

      /*
        `-spec keysort(pos_integer(), [Tuple]) -> [Tuple]`
        > where Tuple is validated to be <: AnyTupleType
       */
      case RemoteId("lists", "keysort", 2) =>
        val List(index, tupleList) = args
        val List(indexTy, tupleListTy) = argTys
        def validate(): Unit = {
          if (!subtype.subType(indexTy, NumberType))
            throw ExpectedSubtype(index.pos, index, expected = NumberType, got = indexTy)
          if (!subtype.subType(tupleListTy, ListType(AnyTupleType)))
            throw ExpectedSubtype(tupleList.pos, tupleList, expected = ListType(AnyTupleType), got = tupleListTy)
        }
        validate()
        (tupleListTy, env1)

      /*
        `-spec keystore(term(), pos_integer(), [Tuple], Replacement) -> [Tuple | Replacement].`
        > where Tuple and Replacement to are validated to be <: AnyTupleType
       */
      case RemoteId("lists", "keystore", 4) =>
        val List(key, index, tupleList, replacement) = args
        val List(_keyAny, indexTy, tupleListTy, replacementTy) = argTys
        def validate(): Unit = {
          if (!subtype.subType(indexTy, NumberType))
            throw ExpectedSubtype(index.pos, index, expected = NumberType, got = indexTy)
          if (!subtype.subType(tupleListTy, ListType(AnyTupleType)))
            throw ExpectedSubtype(tupleList.pos, tupleList, expected = ListType(AnyTupleType), got = tupleListTy)
          if (!subtype.subType(replacementTy, AnyTupleType))
            throw ExpectedSubtype(replacement.pos, replacement, expected = AnyTupleType, got = replacementTy)
        }
        validate()
        val ListType(inTupleTy) = narrow.asListType(tupleListTy).get
        val resTy = ListType(subtype.join(inTupleTy, replacementTy))
        (resTy, env1)

      case RemoteId("maps", "filter", 2) =>
        val List(funArg, collection) = args
        val List(funArgTy, collectionTy) = argTys
        val (keyTy, valTy) = unpackMapOrIterTy(collectionTy, collection.pos)
          .getOrElse(
            throw ExpectedSubtype(collection.pos, collection, expected = mapOrIter, got = collectionTy)
          )
        val expFunTy = FunType(Nil, List(keyTy, valTy), booleanType)
        funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, expFunTy, env)
          case _ =>
            if (!subtype.subType(funArgTy, expFunTy))
              throw ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy)
        }
        (DictMap(keyTy, valTy), env1)

      case RemoteId("maps", "find", 2) =>
        val List(key, map) = args
        val List(keyTy, mapTy) = argTys
        val anyMapTy = DictMap(AnyType, AnyType)
        if (!subtype.subType(mapTy, anyMapTy))
          throw ExpectedSubtype(map.pos, map, expected = anyMapTy, got = mapTy)
        val mapType = narrow.asMapType(mapTy)
        val valTy = keyTy match {
          case AtomLitType(key) =>
            narrow.getValType(key, mapType)
          case _ =>
            narrow.getValType(mapType)
        }
        val resTy = UnionType(Set(TupleType(List(AtomLitType("ok"), valTy)), AtomLitType("error")))
        (resTy, env1)

      case RemoteId("maps", "fold", 3) =>
        val List(funArg, _acc, collection) = args
        val List(funArgTy, accTy1, collectionTy) = argTys
        val (keyTy, valTy) = unpackMapOrIterTy(collectionTy, collection.pos)
          .getOrElse(
            throw ExpectedSubtype(collection.pos, collection, expected = mapOrIter, got = collectionTy)
          )
        def getAccumulatorTy(accTy: Type): Type = funArg match {
          case lambda: Lambda =>
            val vTys = lambda.clauses.map(elab.elabClause(_, List(keyTy, valTy, accTy), env, Set.empty)).map(_._1)
            subtype.join(accTy :: vTys)
          case _ =>
            val expFunTy = FunType(Nil, List(keyTy, valTy, accTy), AnyType)
            if (!subtype.subType(funArgTy, expFunTy))
              throw ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy)

            val vTys = narrow.asFunType(funArgTy, 3).get.map(_.resTy)
            subtype.join(accTy :: vTys)
        }
        def validateAccumulatorTy(accTy: Type): Unit = funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, FunType(Nil, List(keyTy, valTy, accTy), accTy), env)
          case _ =>
            val expFunTy = FunType(Nil, List(keyTy, valTy, accTy), accTy)
            if (!subtype.subType(funArgTy, expFunTy))
              throw ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy)
        }
        val accTy2 = getAccumulatorTy(accTy1)
        val accTy3 = getAccumulatorTy(accTy2)
        validateAccumulatorTy(accTy3)
        (accTy3, env1)

      case RemoteId("maps", "get", 2) =>
        val List(key, map) = args
        val List(keyTy, mapTy) = argTys
        val anyMapTy = DictMap(AnyType, AnyType)
        if (!subtype.subType(mapTy, anyMapTy))
          throw ExpectedSubtype(map.pos, map, expected = anyMapTy, got = mapTy)
        val mapType = narrow.asMapType(mapTy)
        keyTy match {
          case AtomLitType(key) =>
            val valTy = narrow.getValType(key, mapType)
            (valTy, env1)
          case _ =>
            val valTy = narrow.getValType(mapType)
            (valTy, env1)
        }

      case RemoteId("maps", "get", 3) =>
        val List(key, map, defaultVal) = args
        val List(keyTy, mapTy, defaultValTy) = argTys
        val anyMapTy = DictMap(AnyType, AnyType)
        if (!subtype.subType(mapTy, anyMapTy))
          throw ExpectedSubtype(map.pos, map, expected = anyMapTy, got = mapTy)
        val mapType = narrow.asMapType(mapTy)
        keyTy match {
          case AtomLitType(key) =>
            val valTy = narrow.getValType(key, mapType)
            (subtype.join(valTy, defaultValTy), env1)
          case _ =>
            val valTy = narrow.getValType(mapType)
            (subtype.join(valTy, defaultValTy), env1)
        }

      case fqn @ RemoteId("maps", "put", 3) =>
        val List(key, _, map) = args
        key match {
          case AtomLit(keyAtom) =>
            val List(_, valTy, mapTy) = argTys
            val anyMap = DictMap(AnyType, AnyType)
            if (!subtype.subType(mapTy, anyMap)) {
              throw ExpectedSubtype(map.pos, map, expected = anyMap, got = mapTy)
            }
            val resTy = narrow.adjustMapType(mapTy, AtomLitType(keyAtom), valTy)
            (resTy, env1)
          case _ =>
            val ft = util.getFunType(fqn).get
            val resTy = elabApply.elabApply(ft, args, argTys, env1)
            (resTy, env1)
        }

      case RemoteId("maps", "map", 2) =>
        val List(funArg, collection) = args
        val List(funArgTy, collectionTy) = argTys
        val (keyTy, valTy) = unpackMapOrIterTy(collectionTy, collection.pos)
          .getOrElse(
            throw ExpectedSubtype(collection.pos, collection, expected = mapOrIter, got = collectionTy)
          )
        val resValTy = funArg match {
          case lambda: Lambda =>
            val vTys = lambda.clauses.map(elab.elabClause(_, List(keyTy, valTy), env, Set.empty)).map(_._1)
            subtype.join(vTys)
          case _ =>
            val expFunTy = FunType(Nil, List(keyTy, valTy), AnyType)
            if (!subtype.subType(funArgTy, expFunTy))
              throw ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy)

            val vTys = narrow.asFunType(funArgTy, 2).get.map(_.resTy)
            subtype.join(vTys)
        }
        (DictMap(keyTy, resValTy), env1)

      case RemoteId("maps", "remove", 2) =>
        val List(keyArg, mapArg) = args
        val List(keyTy, mapTy) = argTys
        if (!subtype.subType(mapTy, anyMapTy)) {
          throw ExpectedSubtype(mapArg.pos, mapArg, expected = anyMapTy, got = mapTy)
        }
        def remove(mapTy: Type): Type = mapTy match {
          case shape: ShapeMap =>
            keyTy match {
              case AtomLitType(key) =>
                val props = shape.props.filter(_.key != key)
                shape.copy(props = props)
              case _ =>
                val valTy = subtype.join(shape.props.map(_.tp))
                DictMap(AtomType, valTy)
            }
          case UnionType(tys) =>
            subtype.join(tys.map(remove))
          case dict: DictMap =>
            dict
          case NoneType =>
            NoneType
          case unexpected =>
            // $COVERAGE-OFF$
            throw new IllegalStateException(s"unexpected non-map $unexpected")
          // $COVERAGE-ON$
        }

        val ty = remove(narrow.asMapType(mapTy))
        (ty, env1)

      case RemoteId("maps", "with", 2) =>
        @tailrec
        def toKey(ty: Type)(implicit pos: Pos): Option[String] = ty match {
          case AtomLitType(key) => Some(key)
          case UnionType(_) =>
            None
          case RemoteType(rid, argTys) =>
            // Note: this won't infinite-loop since we don't continue under type constructors
            toKey(util.getTypeDeclBody(rid, argTys))
          case _ =>
            None
        }
        @tailrec
        def getKeysToKeep(e: Expr, keys: List[String]): Option[List[String]] = {
          e match {
            case Exprs.NilLit() =>
              Some(keys)
            case Exprs.Cons(h, t) =>
              toKey(elab.elabExpr(h, env)._1)(h.pos) match {
                case Some(key) =>
                  getKeysToKeep(t, key :: keys)
                case None =>
                  None
              }
            case _ => None
          }
        }
        val List(keysArg, mapArg) = args
        val List(keysTy, mapTy) = argTys
        if (!subtype.subType(mapTy, anyMapTy)) {
          throw ExpectedSubtype(mapArg.pos, mapArg, expected = anyMapTy, got = mapTy)
        }
        val expKeysTy = ListType(AnyType)
        if (!subtype.subType(keysTy, expKeysTy)) {
          throw ExpectedSubtype(keysArg.pos, keysArg, expected = expKeysTy, got = keysTy)
        }
        def `with`(mapTy: Type): Type = mapTy match {
          case shape: ShapeMap =>
            getKeysToKeep(keysArg, Nil).map(_.toSet) match {
              case None =>
                val valTy = subtype.join(shape.props.map(_.tp))
                DictMap(AtomType, valTy)
              case Some(keysToKeep) =>
                val props = shape.props.filter(prop => keysToKeep(prop.key))
                shape.copy(props = props)
            }
          case UnionType(tys) =>
            subtype.join(tys.map(`with`))
          case dict: DictMap =>
            dict
          case NoneType =>
            NoneType
          case unexpected =>
            // $COVERAGE-OFF$
            throw new IllegalStateException(s"unexpected non-map $unexpected")
          // $COVERAGE-ON$

        }
        val ty = `with`(narrow.asMapType(mapTy))
        (ty, env1)

      case RemoteId("maps", "without", 2) =>
        @tailrec
        def toKey(ty: Type)(implicit pos: Pos): Option[String] = ty match {
          case AtomLitType(key) => Some(key)
          case UnionType(_) =>
            None
          case RemoteType(rid, argTys) =>
            // Note: this won't infinite-loop since we don't continue under type constructors
            toKey(util.getTypeDeclBody(rid, argTys))
          case _ =>
            None
        }
        @tailrec
        def getKeysToRemove(e: Expr, keys: List[String]): Option[List[String]] = {
          e match {
            case Exprs.NilLit() =>
              Some(keys)
            case Exprs.Cons(h, t) =>
              toKey(elab.elabExpr(h, env)._1)(h.pos) match {
                case Some(key) =>
                  getKeysToRemove(t, key :: keys)
                case None =>
                  None
              }
            case _ => None
          }
        }
        val List(keysArg, mapArg) = args
        val List(keysTy, mapTy) = argTys
        if (!subtype.subType(mapTy, anyMapTy)) {
          throw ExpectedSubtype(mapArg.pos, mapArg, expected = anyMapTy, got = mapTy)
        }
        val expKeysTy = ListType(AnyType)
        if (!subtype.subType(keysTy, expKeysTy)) {
          throw ExpectedSubtype(keysArg.pos, keysArg, expected = expKeysTy, got = keysTy)
        }
        def without(mapTy: Type): Type = mapTy match {
          case shape: ShapeMap =>
            getKeysToRemove(keysArg, Nil).map(_.toSet) match {
              case None =>
                val valTy = subtype.join(shape.props.map(_.tp))
                DictMap(AtomType, valTy)
              case Some(keysToRemove) =>
                val props = shape.props.filterNot(prop => keysToRemove(prop.key))
                shape.copy(props = props)
            }
          case UnionType(tys) =>
            subtype.join(tys.map(without))
          case dict: DictMap =>
            dict
          case NoneType =>
            NoneType
          case unexpected =>
            // $COVERAGE-OFF$
            throw new IllegalStateException(s"unexpected non-map $unexpected")
          // $COVERAGE-ON$

        }
        val ty = without(narrow.asMapType(mapTy))
        (ty, env1)

      // $COVERAGE-OFF$
      case rid =>
        throw new IllegalArgumentException(s"unexpected $rid")
      // $COVERAGE-ON$
    }
  }

  private lazy val anyItTy = RemoteType(RemoteId("maps", "iterator", 2), List(AnyType, AnyType))
  private lazy val anyMapTy = DictMap(AnyType, AnyType)
  private lazy val mapOrIter = subtype.join(anyItTy, anyMapTy)

  private def unpackMapOrIterTy(ty: Type, pos: Pos): Option[(Type, Type)] =
    if (!subtype.subType(ty, mapOrIter)) None
    else {
      ty match {
        case UnionType(tys) =>
          val kvs = for {
            kvOpts <- tys.map(unpackMapOrIterTy(_, pos))
            (k, v) <- kvOpts
          } yield (k, v)
          val (keyTys, valTys) = kvs.unzip
          Some(subtype.join(keyTys), subtype.join(valTys))
        case OpaqueType(RemoteId("maps", "iterator", 2), List(kTy, valTy)) =>
          // $COVERAGE-OFF$
          Some(kTy, valTy)
        // $COVERAGE-ON$
        case RemoteType(RemoteId("maps", "iterator", 2), List(kTy, valTy)) =>
          Some(kTy, valTy)
        case _ =>
          val mapTy = narrow.asMapType(ty)
          val k = narrow.getKeyType(mapTy)
          val v = narrow.getValType(mapTy)
          Some(k, v)
      }
    }

}
