/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.{AtomLit, Cons, Expr, IntLit, Lambda, NilLit, Var}
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.{Exprs, Pos, RemoteId}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{ExpectedSubtype, IndexOutOfBounds, UnboundRecord}
import com.whatsapp.eqwalizer.ast.CompilerMacro
import com.whatsapp.eqwalizer.ast.Pats.{Pat, PatAtom, PatTuple, PatVar, PatWild}
import com.whatsapp.eqwalizer.ast.Types.Key.asType

import scala.collection.mutable

class ElabApplyCustom(pipelineContext: PipelineContext) {
  private lazy val elab = pipelineContext.elab
  private lazy val elabApply = pipelineContext.elabApply
  private lazy val elabPat = pipelineContext.elabPat
  private lazy val check = pipelineContext.check
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val occurrence = pipelineCtx.occurrence
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  private lazy val typeInfo = pipelineCtx.typeInfo
  implicit val pipelineCtx: PipelineContext = pipelineContext

  implicit class JoinableTypes[T <: Type](tys: Set[T]) {
    def join(): Type = {
      subtype.join(tys)
    }

    def join(t: Type): Type = {
      subtype.join(t, tys)
    }
  }

  private type CustomHandler = (List[Expr], List[Type], Env, Env, Pos) => (Type, Env)
  private type CustomPredicateHandler = (List[Expr], List[Type], Env, Pos) => (Type, Env, Env)

  def isCustom(remoteId: RemoteId): Boolean =
    custom.contains(remoteId)

  def isCustomPredicate(id: RemoteId): Boolean =
    customPredicate.contains(id)

  def elabCustom(remoteId: RemoteId, args: List[Expr], env: Env, callPos: Pos): (Type, Env) = {
    val (argTys, env1) = typeInfo.withoutLambdaTypeCollection { elab.elabExprs(args, env) }
    custom(remoteId)(args, argTys, env, env1, callPos)
  }

  def elabCustomPredicate(remoteId: RemoteId, args: List[Expr], env: Env, callPos: Pos): (Type, Env, Env) = {
    val (argTys, env1) = elab.elabExprs(args, env)
    customPredicate(remoteId)(args, argTys, env1, callPos)
  }

  private def coerce(expr: Expr, ty: Type, expTy: Type): Type = {
    if (!subtype.subType(ty, expTy)) {
      diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expTy, ty))
      DynamicType
    } else {
      ty
    }
  }

  private def coerceToMaps(expr: Expr, ty: Type): Set[MapType] = {
    val coercedTy = coerce(expr, ty, anyMapTy)
    narrow.asMapTypes(coercedTy)
  }

  private def coerceToMapsOrIter(expr: Expr, ty: Type): Set[MapType] = {
    val mapOrIterTy = UnionType(Set(anyMapTy, RemoteType(RemoteId("maps", "iterator", 0), List())))
    val coercedTy = coerce(expr, ty, mapOrIterTy)
    narrow.asMapOrIterTypes(coercedTy)
  }

  private lazy val custom: Map[RemoteId, CustomHandler] = {
    // Shared by maps:get/2 and erlang:map_get/2
    val mapsGetHandler: CustomHandler = { (args, argTys, _, env1, _) =>
      val List(key, map) = args
      val List(keyTy, mapTy) = argTys
      val mapTys = coerceToMaps(map, mapTy)
      val staticKeys = narrow.asKeys(keyTy)
      val valTys = staticKeys match {
        case Some(keys) =>
          keys.flatMap(k => mapTys.map(narrow.getValType(k, _)))
        case None =>
          mapTys.map(narrow.getValType)
      }
      (valTys.join(), env1)
    }

    // Shared by lists:flatten/1 and lists:flatten/2
    def flattenHandler(arity: Int): CustomHandler = { (args, argTys, _, env1, _) =>
      val arg = args.head
      val anyListTy = ListType(AnyType)
      val argTy = coerce(arg, argTys.head, anyListTy)
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
          val tailTy = coerce(tail, argTys(1), anyListTy)
          val Some(ListType(tailElemTy)) = narrow.asListType(tailTy): @unchecked
          subtype.join(argElemTy, tailElemTy)
      }
      (ListType(elemTy), env1)
    }

    Map[RemoteId, CustomHandler](
      RemoteId("file", "open", 2) -> { (args, argTys, _, env1, callPos) =>
        val ft = util.getFunType(RemoteId("file", "open", 2))
        val resTy = elabApply.elabApply(ft, args, argTys, env1, callPos)
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
          } else resTy
        }
        (resTyPrecise, env1)
      },
      RemoteId("lists", "filtermap", 2) -> { (args, argTys, env, env1, callPos) =>
        val List(funArg, collection) = args
        val List(funArgTy, collectionTy) = argTys
        val listAnyTy = ListType(AnyType)
        val collectionTyCoerced = coerce(collection, collectionTy, listAnyTy)
        val elemTy = narrow.asListType(collectionTyCoerced).get.t
        val tupleTrueAnyTy = TupleType(List(trueType, AnyType))
        val expRet = subtype.join(booleanType, tupleTrueAnyTy)
        val expFunTy = FunType(0, List(elemTy), expRet)
        val funResTys = funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, expFunTy, env)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            val clauseEnvs = occurrence.clausesEnvs(lambda.clauses, List(elemTy), lamEnv)
            lambda.clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => elab.elabClause(clause, List(elemTy), occEnv, Set.empty)._1)
          case _ =>
            if (!subtype.subType(funArgTy, expFunTy)) {
              diagnosticsInfo.add(ExpectedSubtype(funArg.pos, funArg, expected = expFunTy, got = funArgTy))
              List(DynamicType)
            } else narrow.asFunTypes(funArgTy, 1).map(_.resTy)
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
            DynamicType
        }
        val resItemTy = funResultsToItemTy(funResTys, elemTy, callPos)
        (ListType(resItemTy), env1)
      },
      RemoteId("lists", "flatten", 1) -> flattenHandler(1),
      RemoteId("lists", "flatten", 2) -> flattenHandler(2),
      /*
        `-spec keysort(pos_integer(), [Tuple]) -> [Tuple]`
        > where Tuple is validated to be <: AnyTupleType
       */
      RemoteId("lists", "keysort", 2) -> { (args, argTys, _, env1, _) =>
        val List(index, tupleList) = args
        val List(indexTy, tupleListTy) = argTys
        val _ = coerce(index, indexTy, IntegerType)
        val tupleListCoercedTy = coerce(tupleList, tupleListTy, ListType(AnyTupleType))
        (tupleListCoercedTy, env1)
      },
      /*
        `-spec keystore(term(), pos_integer(), [Tuple], Replacement) -> [Tuple | Replacement].`
        > where Tuple and Replacement to are validated to be <: AnyTupleType
       */
      RemoteId("lists", "keystore", 4) -> { (args, argTys, _, env1, _) =>
        val List(key, index, tupleList, replacement) = args
        val List(_keyAny, indexTy, tupleListTy, replacementTy) = argTys
        val _ = coerce(index, indexTy, IntegerType)
        val tupleListCoercedTy = coerce(tupleList, tupleListTy, ListType(AnyTupleType))
        val replacementCoercedTy = coerce(replacement, replacementTy, AnyTupleType)
        val ListType(inTupleTy) = narrow.asListType(tupleListCoercedTy).get
        val resTy = ListType(subtype.join(inTupleTy, replacementCoercedTy))
        (resTy, env1)
      },
      RemoteId("lists", "partition", 2) -> { (args, argTys, env, env1, _) =>
        val List(pred, list) = args
        val List(predTy, listTy) = argTys
        val coercedListTy = coerce(list, listTy, ListType(AnyType))
        val elemTy = narrow.asListType(coercedListTy).get.t
        val expFunTy = FunType(0, List(elemTy), booleanType)
        pred match {
          case lambda: Lambda if Predicates.booleanReturnClauses(lambda.clauses) =>
            check.checkLambda(lambda, expFunTy, env1)
            val (trueClause, falseClause) = Predicates.getTrueFalseReturnClauses(lambda.clauses)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env1)
            val List(trueEnv, falseEnv) = occurrence.clausesEnvs(List(trueClause, falseClause), List(elemTy), lamEnv)
            val (trueTy, _) = elabPat.elabPat(trueClause.pats.head, elemTy, trueEnv)
            val (falseTy, _) = elabPat.elabPat(falseClause.pats.head, elemTy, falseEnv)
            (TupleType(List(ListType(trueTy), ListType(falseTy))), env1)
          case _ =>
            if (!subtype.subType(predTy, expFunTy)) {
              diagnosticsInfo.add(ExpectedSubtype(pred.pos, pred, expected = expFunTy, got = predTy))
              (TupleType(List(DynamicType, DynamicType)), env1)
            } else (TupleType(List(ListType(elemTy), ListType(elemTy))), env1)
        }
      },
      RemoteId("maps", "filter", 2) -> { (args, argTys, env, env1, _) =>
        val List(funArg, map) = args
        val List(funArgTy, mapTy) = argTys
        val mapTys = coerceToMapsOrIter(map, mapTy)
        val keyTy = mapTys.map(narrow.getKeyType).join()
        val valTy = mapTys.map(narrow.getValType).join()
        val expFunTy = FunType(0, List(keyTy, valTy), booleanType)
        funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, expFunTy, env)
          case _ =>
            coerce(funArg, funArgTy, expFunTy)
        }
        (mapTys.map(narrow.setAllFieldsOptional(_)).join(), env1)
      },
      RemoteId("maps", "find", 2) -> { (args, argTys, _, env1, _) =>
        val List(key, map) = args
        val List(keyTy, mapTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val valTy = narrow.asKeys(keyTy) match {
          case Some(keys) =>
            keys.flatMap(k => mapTys.map(narrow.getValType(k, _))).join()
          case None =>
            mapTys.map(narrow.getValType).join()
        }
        val resTy = UnionType(Set(TupleType(List(AtomLitType("ok"), valTy)), AtomLitType("error")))
        (resTy, env1)
      },
      RemoteId("maps", "intersect", 2) -> { (args, argTys, _, env1, _) =>
        val List(map1, map2) = args
        val List(ty1, ty2) = argTys
        val mapTys1 = coerceToMaps(map1, ty1)
        val mapTys2 = coerceToMaps(map2, ty2)
        val reqKeys = mapTys1.map(narrow.getKeyType(_)(reqOnly = true)).join()
        val allKeys = mapTys1.map(narrow.getKeyType).join()
        (mapTys2.map(narrow.selectKeys(reqKeys, allKeys, _)).join(), env1)
      },
      RemoteId("maps", "fold", 3) -> { (args, argTys, env, env1, _) =>
        val List(funArg, _acc, map) = args
        val List(funArgTy, accTy1, mapTy) = argTys
        val mapTys = coerceToMapsOrIter(map, mapTy)
        val keyTy = mapTys.map(narrow.getKeyType).join()
        val valTy = mapTys.map(narrow.getValType).join()
        def isShapeIterator(lambda: Lambda): Boolean = {
          val usedKeys = mutable.Set[String]()
          (lambda.clauses forall { clause =>
            clause.pats match {
              case List(PatAtom(a), _, _) if !usedKeys(a) =>
                usedKeys.add(a)
                true
              case List(PatVar(_) | PatWild(), _, _) =>
                true
              case _ =>
                false
            }
          }) && (lambda.clauses.count { clause =>
            clause.pats match {
              case List(PatVar(_) | PatWild(), _, _) =>
                true
              case _ =>
                false
            }
          } <= 1)
        }

        def getAccumulatorTys(accTy: Type): List[Type] = funArg match {
          case lambda: Lambda if isShapeIterator(lambda) =>
            val expFunTy = FunType(0, List(keyTy, valTy, accTy), accTy)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            val occEnvs = occurrence.clausesEnvs(lambda.clauses, List(keyTy, valTy, accTy), lamEnv)
            var keyTyLast = keyTy
            val vTys = lambda.clauses.lazyZip(occEnvs).map { (clause, occEnv) =>
              clause.pats.head match {
                case PatAtom(a) =>
                  val refinedValTy = UnionType(mapTys.map(m => narrow.getValType(AtomKey(a), m)))
                  keyTyLast = occurrence.remove(keyTyLast, AtomLitType(a))
                  elab.elabClause(clause, List(AtomLitType(a), refinedValTy, accTy), occEnv, Set.empty)._1
                case _ =>
                  elab.elabClause(clause, List(keyTyLast, valTy, accTy), occEnv, Set.empty)._1
              }
            }
            accTy1 :: vTys
          case lambda: Lambda =>
            val expFunTy = FunType(0, List(keyTy, valTy, accTy), accTy)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            val occEnvs = occurrence.clausesEnvs(lambda.clauses, List(keyTy, valTy, accTy), lamEnv)
            val vTys =
              lambda.clauses
                .lazyZip(occEnvs)
                .map((clause, occEnv) => elab.elabClause(clause, List(keyTy, valTy, accTy), occEnv, Set.empty))
                .map(_._1)
            accTy1 :: vTys
          case _ =>
            val expFunTy = FunType(0, List(keyTy, valTy, accTy), AnyType)
            val funArgCoercedTy = coerce(funArg, funArgTy, expFunTy)
            val vTys = narrow.asFunTypes(funArgCoercedTy, 3).map(_.resTy).toList
            accTy1 :: vTys
        }
        def validateAccumulatorTy(accTy: Type): Unit = funArg match {
          case lambda: Lambda if isShapeIterator(lambda) =>
            val expFunTy = FunType(0, List(keyTy, valTy, accTy), accTy)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            val occEnvs = occurrence.clausesEnvs(lambda.clauses, List(keyTy, valTy, accTy), lamEnv)
            var keyTyLast = keyTy
            lambda.clauses.lazyZip(occEnvs).map { (clause, occEnv) =>
              clause.pats.head match {
                case PatAtom(a) =>
                  val refinedValTy = UnionType(mapTys.map(m => narrow.getValType(AtomKey(a), m)))
                  keyTyLast = occurrence.remove(keyTyLast, AtomLitType(a))
                  check.checkClause(clause, List(AtomLitType(a), refinedValTy, accTy), accTy, occEnv, Set.empty)
                case _ =>
                  check.checkClause(clause, List(keyTyLast, valTy, accTy), accTy, occEnv, Set.empty)
              }
            }
          case lambda: Lambda =>
            check.checkLambda(lambda, FunType(0, List(keyTy, valTy, accTy), accTy), env)
          case _ =>
            val expFunTy = FunType(0, List(keyTy, valTy, accTy), accTy)
            coerce(funArg, funArgTy, expFunTy)
        }
        val accTys2 = getAccumulatorTys(accTy1)
        val accTy3 = subtype.join(getAccumulatorTys(narrow.joinAndMergeMaps(accTys2)))
        validateAccumulatorTy(accTy3)
        (accTy3, env1)
      },
      RemoteId("maps", "get", 2) -> mapsGetHandler,
      RemoteId("erlang", "map_get", 2) -> mapsGetHandler,
      /*
        `-spec element(N :: NumberType, Tup :: TupleType) -> Out`, where `Out` is:
          - `Tup[N]` when `N` is an integer literal corresponding to a valid index
          - Union of element types of `Tup` when `N` is not a literal
          - An error otherwise (index out of bounds or unexpected type)
       */
      RemoteId("erlang", "element", 2) -> { (args, argTys, _, env1, callPos) =>
        val List(index, tuple) = args
        val List(indexTy, tupleTy) = argTys
        val _ = coerce(index, indexTy, IntegerType)
        val tupleCoercedTy = coerce(tuple, tupleTy, AnyTupleType)

        val elemTy = index match {
          case IntLit(Some(n)) =>
            narrow.getTupleElement(tupleCoercedTy, n) match {
              case Right(elemTy) => elemTy
              case Left(tupLen) =>
                diagnosticsInfo.add(IndexOutOfBounds(callPos, index, n, tupLen))
                DynamicType
            }
          case _ =>
            narrow.getAllTupleElements(tupleCoercedTy)
        }

        (elemTy, env1)
      },
      RemoteId("erlang", "setelement", 3) -> { (args, argTys, _, env1, callPos) =>
        val List(index, tuple, elem) = args
        val List(indexTy, argTy, elemTy) = argTys
        val _ = coerce(index, indexTy, IntegerType)
        val tupleTy = coerce(tuple, argTy, AnyTupleType)

        val resTy = index match {
          case IntLit(Some(n)) =>
            narrow.setTupleElement(tupleTy, n, elemTy) match {
              case Right(t) => t
              case Left(tupLen) =>
                diagnosticsInfo.add(IndexOutOfBounds(callPos, index, n, tupLen))
                DynamicType
            }
          case _ =>
            AnyTupleType
        }

        (resTy, env1)
      },
      RemoteId("maps", "get", 3) -> { (args, argTys, _, env1, _) =>
        val List(key, map, defaultVal) = args
        val List(keyTy, mapTy, defaultValTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val staticKeys = narrow.asKeys(keyTy)
        val valTys =
          staticKeys match {
            case Some(keys) =>
              keys.flatMap(k => mapTys.map(narrow.getValType(k, _)))
            case None =>
              mapTys.map(narrow.getValType)
          }
        (valTys.join(defaultValTy), env1)
      },
      RemoteId("maps", "put", 3) -> { (args, argTys, _, env1, _) =>
        val List(_, _, map) = args
        val List(keyTy, valTy, mapTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val adjustedTys = mapTys.map(narrow.adjustMapType(_, keyTy, valTy))
        (adjustedTys.join(), env1)
      },
      RemoteId("maps", "map", 2) -> { (args, argTys, env, env1, _) =>
        val List(funArg, map) = args
        val List(funArgTy, mapTy) = argTys
        val mapTys = coerceToMapsOrIter(map, mapTy)
        val keyTy = mapTys.map(narrow.getKeyType).join()
        val valTy = mapTys.map(narrow.getValType).join()
        val expFunTy = FunType(0, List(keyTy, valTy), AnyType)
        def mapValueType(resValTy: Type, argMapTy: MapType): Type = {
          MapType(
            argMapTy.props.map { case (key, Prop(req, _)) =>
              (key, Prop(req, resValTy))
            },
            argMapTy.kType,
            resValTy,
          )
        }
        def elabMap(lambda: Lambda, argMapTy: MapType, env: Env): MapType = {
          MapType(
            argMapTy.props.map { case (key, prop) =>
              val resTy = elabLambda2(lambda, asType(key), prop.tp, env)
              (key, Prop(prop.req, resTy))
            },
            argMapTy.kType,
            elabLambda2(lambda, argMapTy.kType, argMapTy.vType, env),
          )
        }
        funArg match {
          case lambda: Lambda =>
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            (mapTys.map(elabMap(lambda, _, lamEnv)).join(), env1)
          case _ =>
            val funArgCoercedTy = coerce(funArg, funArgTy, expFunTy)
            val vTys = narrow.asFunTypes(funArgCoercedTy, 2).map(_.resTy)
            (mapTys.map(mapValueType(subtype.join(vTys), _)).join(), env1)
        }
      },
      RemoteId("maps", "filtermap", 2) -> { (args, argTys, env, env1, callPos) =>
        val List(funArg, map) = args
        val List(funArgTy, mapTy) = argTys
        val mapTys = coerceToMapsOrIter(map, mapTy)
        val keyTy = mapTys.map(narrow.getKeyType).join()
        val valTy = mapTys.map(narrow.getValType).join()
        val tupleTrueAnyTy = TupleType(List(trueType, AnyType))
        val expRet = subtype.join(booleanType, tupleTrueAnyTy)
        val expFunTy = FunType(0, List(keyTy, valTy), expRet)
        val funResTys = funArg match {
          case lambda: Lambda =>
            check.checkLambda(lambda, expFunTy, env)
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            val clauseEnvs = occurrence.clausesEnvs(lambda.clauses, List(keyTy, valTy), lamEnv)
            lambda.clauses
              .lazyZip(clauseEnvs)
              .map((clause, occEnv) => elab.elabClause(clause, List(keyTy, valTy), occEnv, Set.empty)._1)
          case _ =>
            val funArgCoercedTy = coerce(funArg, funArgTy, expFunTy)
            narrow.asFunTypes(funArgCoercedTy, 2).map(_.resTy)
        }
        def funResultsToValTy(tys: Iterable[Type], defaultTy: Type, pos: Pos): Type =
          tys.foldLeft(NoneType: Type)((memo, ty) => subtype.join(memo, funResultToValTy(ty, defaultTy, pos)))
        def funResultToValTy(ty: Type, defaultTy: Type, pos: Pos): Type = ty match {
          case UnionType(tys) =>
            funResultsToValTy(tys, defaultTy, pos)
          case TupleType(List(t1, resElemTy)) if subtype.subType(t1, trueType) =>
            resElemTy
          case ty if subtype.subType(ty, falseType) =>
            NoneType
          case ty if subtype.subType(ty, booleanType) =>
            defaultTy
          case _ =>
            DynamicType
        }
        val resItemTy = funResultsToValTy(funResTys, valTy, callPos)
        (mapTys.map(narrow.setAllFieldsOptional(_, Some(resItemTy))).join(), env1)
      },
      RemoteId("maps", "foreach", 2) -> { (args, argTys, env, _, _) =>
        val List(funArg, map) = args
        val List(funArgTy, mapTy) = argTys
        val mapTys = coerceToMapsOrIter(map, mapTy)
        val keyTy = mapTys.map(narrow.getKeyType).join()
        val valTy = mapTys.map(narrow.getValType).join()
        def isShapeKey(pat: Pat): Boolean = {
          pat match {
            case PatAtom(_)     => true
            case PatTuple(pats) => pats.forall(isShapeKey)
            case _              => false
          }
        }
        def asKey(pat: Pat): Key = {
          pat match {
            case PatAtom(a)     => AtomKey(a)
            case PatTuple(pats) => TupleKey(pats.map(asKey))
            case _              => throw new IllegalStateException(s"unexpected pattern: $pat")
          }
        }
        def isShapeIterator(lambda: Lambda): Boolean = {
          val usedKeys = mutable.Set[Pat]()
          (lambda.clauses forall { clause =>
            clause.pats match {
              case List(pat, _) if isShapeKey(pat) && !usedKeys(pat) =>
                usedKeys.add(pat)
                true
              case List(PatVar(_) | PatWild(), _) =>
                true
              case _ =>
                false
            }
          }) && (lambda.clauses.count { clause =>
            clause.pats match {
              case List(PatVar(_) | PatWild(), _, _) =>
                true
              case _ =>
                false
            }
          } <= 1)
        }

        val expFunTy = FunType(0, List(keyTy, valTy), AnyType)
        var keyTyLast = keyTy
        funArg match {
          case lambda: Lambda if isShapeIterator(lambda) =>
            val lamEnv = lambda.name.map(name => env.updated(name, expFunTy)).getOrElse(env)
            lambda.clauses.foreach { clause =>
              if (isShapeKey(clause.pats.head)) {
                val key = asKey(clause.pats.head)
                val refinedValTy = UnionType(mapTys.map(m => narrow.getValType(key, m)))
                val kTy = Key.asType(key)
                keyTyLast = occurrence.remove(keyTyLast, kTy)
                check.checkClause(clause, List(kTy, refinedValTy), AnyType, lamEnv, Set.empty)
              } else {
                check.checkClause(clause, List(keyTyLast, valTy), AnyType, lamEnv, Set.empty)
              }
            }
          case _ =>
            check.checkExpr(funArg, expFunTy, env)
        }
        (AtomLitType("ok"), env)
      },
      RemoteId("maps", "remove", 2) -> { (args, argTys, _, env1, _) =>
        val List(keyArg, map) = args
        val List(keyTy, mapTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val key = Key.fromType(keyTy)
        def remove(mapTy: Type): Type = mapTy match {
          case mapType: MapType =>
            key match {
              case Some(k) => mapType.copy(props = mapType.props.removed(k))
              case _       => narrow.setAllFieldsOptional(mapType, None)
            }
          case UnionType(tys) =>
            subtype.join(tys.map(remove))
          case NoneType =>
            NoneType
          case unexpected =>
            throw new IllegalStateException(s"unexpected non-map $unexpected")
        }
        val ty = mapTys.map(remove).join()
        (ty, env1)
      },
      RemoteId("maps", "to_list", 1) -> { (args, argTys, _, env1, _) =>
        val mapTys = coerceToMapsOrIter(args.head, argTys.head)
        val pairTys = mapTys.flatMap(narrow.getKVType)
        (ListType(pairTys.join()), env1)
      },
      RemoteId("maps", "with", 2) -> { (args, argTys, _, env1, _) =>
        val List(keysArg, map) = args
        val List(keysTy, mapTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val keysCoercedTy = coerce(keysArg, keysTy, ListType(AnyType))
        def `with`(mapTy: MapType, keysToKeep: Set[Key]): Type = {
          val props = keysToKeep.flatMap { key =>
            mapTy.props.get(key) match {
              case Some(vProp)                                           => Some(key -> vProp)
              case None if mapTy.kType == DynamicType                    => Some(key -> Prop(true, mapTy.vType))
              case None if subtype.subType(Key.asType(key), mapTy.kType) => Some(key -> Prop(false, mapTy.vType))
              case None                                                  => None
            }
          }
          MapType(props.toMap, NoneType, NoneType)
        }
        asKeySetStrict(keysCoercedTy) match {
          case Some(keys) => (mapTys.map(`with`(_, keys)).join(), env1)
          case None       => (mapTys.map(narrow.setAllFieldsOptional(_)).join(), env1)
        }
      },
      RemoteId("maps", "without", 2) -> { (args, argTys, _, env1, _) =>
        val List(keysArg, map) = args
        val List(keysTy, mapTy) = argTys
        val mapTys = coerceToMaps(map, mapTy)
        val keysCoercedTy = coerce(keysArg, keysTy, ListType(AnyType))
        def without(mapTy: MapType, keysToRemove: Set[Key]): Type =
          mapTy.copy(props = mapTy.props.removedAll(keysToRemove))
        asKeySetStrict(keysCoercedTy) match {
          case Some(keys) => (mapTys.map(without(_, keys)).join(), env1)
          case None       => (mapTys.map(narrow.setAllFieldsOptional(_)).join(), env1)
        }
      },
      RemoteId(CompilerMacro.fake_module, "record_info", 2) -> { (args, _, _, env1, _) =>
        val List(AtomLit(access), name) = args: @unchecked
        val AtomLit(recName) = name: @unchecked

        access match {
          case "size" => (IntegerType, env1)
          case "fields" =>
            val record = util.getRecord(pipelineContext.module, recName)
            record match {
              case Some(recDecl) =>
                val fields = recDecl.fields.map(f => AtomLitType(f.name))
                (ListType(UnionType(fields.toSet)), env1)
              case None =>
                diagnosticsInfo.add(UnboundRecord(name.pos, recName))
                (DynamicType, env1)
            }
        }
      },
      RemoteId("maps", "merge", 2) -> { (args, argTys, _, env1, _) =>
        val List(map1, map2) = args
        val List(ty1, ty2) = argTys
        val mapTys1 = coerceToMaps(map1, ty1)
        val mapTys2 = coerceToMaps(map2, ty2)
        val pairs = for (t1 <- mapTys1; t2 <- mapTys2) yield (t1, t2)
        val resTys = pairs.map { case (ty1, ty2) => narrow.joinAndMergeMaps(List(ty1, ty2), inOrder = true) }
        (resTys.join(), env1)
      },
    )
  }

  private lazy val customPredicate: Map[RemoteId, CustomPredicateHandler] = {
    // Shared by io_lib:char_list/1 and io_lib:latin1_char_list/1
    val ioListPredicateHandler: CustomPredicateHandler = { (args, _, env1, _) =>
      val List(arg) = args
      arg match {
        case Var(x) =>
          val posEnv = env1.updated(x, stringType)
          (booleanType, posEnv, env1)
        case _ =>
          (booleanType, env1, env1)
      }
    }
    Map[RemoteId, CustomPredicateHandler](
      RemoteId("lists", "member", 2) -> { (args, argTys, env1, _) =>
        val List(elem, list) = args
        val List(elemTy, listTy) = argTys
        val listTy1 = coerce(list, listTy, expTy = ListType(AnyType))
        elem match {
          case Var(x) =>
            val ListType(predListTy) = narrow.asListType(listTy1).get
            val posElemT = predListTy
            val xT = env1(x)
            if (subtype.gradualSubType(posElemT, xT)) {
              val posEnv = env1.updated(x, posElemT)
              (booleanType, posEnv, env1)
            } else (booleanType, env1, env1)
          case _ =>
            (booleanType, env1, env1)
        }
      },
      RemoteId("io_lib", "char_list", 1) -> ioListPredicateHandler,
      RemoteId("io_lib", "latin1_char_list", 1) -> ioListPredicateHandler,
    )
  }

  private def asKeySetStrict(ty: Type): Option[Set[Key]] = {
    def go(t: Type): Option[Set[Key]] = t match {
      case UnionType(ts) =>
        ts.foldLeft[Option[Set[Key]]](Some(Set())) { (acc, ty) =>
          acc.flatMap(keys => go(ty).map(keys2 => keys ++ keys2))
        }
      case RemoteType(rid, args) =>
        go(util.getTypeDeclBody(rid, args))
      case NoneType => Some(Set())
      case _        => Key.fromType(t).map(Set(_))
    }
    narrow.asListType(ty).flatMap(lt => go(lt.t))
  }

  private def elabLambda2(lambda: Lambda, t1: Type, t2: Type, env: Env): Type = {
    val clauseEnvs = occurrence.clausesEnvs(lambda.clauses, List(t1, t2), env)
    val resTys =
      lambda.clauses
        .lazyZip(clauseEnvs)
        .map((clause, occEnv) => elab.elabClause(clause, List(t1, t2), occEnv, Set.empty, checkReachability = true)._1)
    subtype.join(resTys)
  }

  private lazy val anyMapTy = MapType(Map(), AnyType, AnyType)
}
