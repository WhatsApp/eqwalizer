/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Guards._
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.{Id, Types}
import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.ast.Types._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Occurrence {
  sealed trait Prop
  case object Unknown extends Prop
  case object True extends Prop
  case object False extends Prop
  case class Pos(obj: Obj, t: Type) extends Prop
  case class Neg(obj: Obj, t: Type) extends Prop
  case class And(props: List[Prop]) extends Prop
  case class Or(props: List[Prop]) extends Prop

  /** Prefer these functions to And.apply and Or.apply */
  def and(props: List[Prop]): Prop = {
    if (props.isEmpty) True
    else if (props.contains(False)) False
    else {
      val flattenedProps = props.flatMap {
        case And(l) => l
        case True   => List()
        case p      => List(p)
      }
      val (propsUkn, propsNotUkn) = flattenedProps.partition(_ == Unknown)
      val simplProps =
        if (propsUkn.isEmpty) propsNotUkn
        else Unknown :: propsNotUkn
      if (simplProps.isEmpty) True
      else if (simplProps.size == 1) simplProps.head
      else And(simplProps)
    }
  }

  def or(props: List[Prop]): Prop = {
    if (props.isEmpty) False
    else if (props.contains(True)) True
    else {
      val flattenedProps = props.flatMap {
        case Or(l) => l
        case False => List()
        case p     => List(p)
      }
      val (propsUkn, propsNotUkn) = flattenedProps.partition(_ == Unknown)
      val simplProps =
        if (propsUkn.isEmpty) propsNotUkn
        else Unknown :: propsNotUkn
      if (simplProps.isEmpty) False
      else if (simplProps.size == 1) simplProps.head
      else Or(simplProps)
    }
  }

  sealed trait Obj
  case class VarObj(v: String) extends Obj
  case class FieldObj(field: Field, obj: Obj) extends Obj

  sealed trait Field
  case class TupleField(index: Int, arity: Option[Int]) extends Field
  case class RecordField(field: String, recName: String) extends Field
  case class MapField(field: Key) extends Field
  case object ListHead extends Field
  case object ListTail extends Field

  type PropEnv = List[Prop]
  type AMap = Map[String, Obj]

  implicit class MaybeOps(maybe: Option[Boolean]) {
    @inline
    def isTrue: Boolean = maybe.contains(true)
    @inline
    def isFalse: Boolean = maybe.contains(false)
  }

  private sealed trait Polarity
  private case object + extends Polarity
  private case object - extends Polarity
  type Path = List[Field]

  val unary_predicates: Map[String, Type] =
    Map(
      "is_atom" -> AtomType,
      "is_binary" -> BinaryType,
      "is_bitstring" -> BinaryType,
      "is_boolean" -> UnionType(Set(falseType, trueType)),
      "is_float" -> floatType,
      "is_function" -> AnyFunType,
      "is_integer" -> NumberType,
      "is_list" -> ListType(AnyType),
      "is_number" -> NumberType,
      "is_pid" -> PidType,
      "is_port" -> PortType,
      "is_reference" -> ReferenceType,
      "is_map" -> MapType(Map(), AnyType, AnyType),
      "is_tuple" -> AnyTupleType,
    )

  sealed trait ValueKind
  case object AtomKind extends ValueKind
  case object BinaryKind extends ValueKind
  case object FunKind extends ValueKind
  case object ListKind extends ValueKind
  case object MapKind extends ValueKind
  case object NumberKind extends ValueKind
  case object PidKind extends ValueKind
  case object PortKind extends ValueKind
  case object ReferenceKind extends ValueKind
  case object TupleKind extends ValueKind

  def kind(t: Type): Option[ValueKind] = t match {
    case AtomLitType(_) | AtomType =>
      Some(AtomKind)
    case BinaryType =>
      Some(BinaryKind)
    case AnyFunType | FunType(_, _, _) | AnyArityFunType(_) =>
      Some(FunKind)
    case NilType | ListType(_) =>
      Some(ListKind)
    case MapType(_, _, _) =>
      Some(MapKind)
    case NumberType =>
      Some(NumberKind)
    case PidType =>
      Some(PidKind)
    case PortType =>
      Some(PortKind)
    case ReferenceType =>
      Some(ReferenceKind)
    case AnyTupleType | TupleType(_) | RecordType(_) | RefinedRecordType(_, _) =>
      Some(TupleKind)
    case _ =>
      None
  }
}

// The main logic of occurrence typing.
final class Occurrence(pipelineContext: PipelineContext) {
  import Occurrence._
  private lazy val module = pipelineContext.module
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val vars = pipelineContext.vars
  private type Name = String
  private var gen = 0
  private def genVar(): String = {
    gen += 1
    s"$$$gen"
  }

  def isEnabled(clauses: List[Clause]): Boolean = {
    val emptyPatterns = clauses.forall(_.pats.isEmpty)
    val shortGuards = clauses.forall(clause => clause.guards.map(guardSize).sum < 32)
    val smallClauses = pipelineContext.unlimitedRefinement || (clauses.size < 7 && shortGuards)
    emptyPatterns || smallClauses
  }

  private def ignoreNumberRefinement(clause: Clause, aMap: AMap, nextClauses: List[(Clause, AMap)]): Boolean = {
    clause match {
      case Clause(_, List(Guard(List(TestCall(Id("is_integer", 1), List(TestVar(v1)))))), _) =>
        nextClauses.exists {
          case (Clause(_, List(Guard(List(TestCall(Id("is_float", 1), List(TestVar(v2)))))), _), aMap2) =>
            aMap.getOrElse(v1, VarObj(v1)) == aMap2.getOrElse(v2, VarObj(v2))
          case _ => false
        }
      case Clause(_, List(Guard(List(TestCall(Id("is_float", 1), List(TestVar(v1)))))), _) =>
        nextClauses.exists {
          case (Clause(_, List(Guard(List(TestCall(Id("is_integer", 1), List(TestVar(v2)))))), _), aMap2) =>
            aMap.getOrElse(v1, VarObj(v1)) == aMap2.getOrElse(v2, VarObj(v2))
          case _ => false
        }
      case _ => false
    }
  }

  private def linearVars(clause: Clause): Boolean = {
    val varsL = vars.clausePatVarsL(clause)
    varsL.toSet.size == varsL.size
  }

  private def guardSize(guard: Guard): Int =
    guard.tests.map(testSize).sum

  private def testSize(test: Test): Int =
    test match {
      case TestUnOp("not", test) =>
        testSize(test)
      case TestBinOp("and" | "andalso", test1, test2) =>
        testSize(test1) + testSize(test2)
      case TestBinOp("or" | "orelse", test1, test2) =>
        testSize(test1) + testSize(test2)
      case _ =>
        1
    }

  // These are specialized methods to upgrade environments/context
  // by occurrence typing
  def ifEnvs(i: If, env: Env): List[Env] = {
    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]
    for (clause <- i.clauses) {
      if (linearVars(clause)) {
        val aMap = Map.empty[Name, Obj]
        val (testPos, testNeg) = guardsProps(clause.guards, aMap)
        val clauseProps = combine(testPos.toList, propsAcc)
        val clauseEnv = batchSelect(env, clauseProps, aMap)
        clauseEnvs.addOne(clauseEnv)
        propsAcc = testNeg match {
          case None =>
            propsAcc
          case Some(neg) =>
            propsAcc :+ neg
        }
      } else {
        clauseEnvs.addOne(env)
      }
    }
    clauseEnvs.toList
  }

  def caseEnvs(c: Case, selType: Type, env: Env): List[Env] = {
    val (env1, x) = c.expr match {
      case Var(n) =>
        (env, n)
      // important for thrift - see D31025723
      case Match(PatVar(n), _) =>
        (env, n)
      case _ =>
        val v = genVar()
        (env + (v -> selType), v)
    }
    val eMap = c.expr match {
      case Tuple(elems) =>
        elems.zipWithIndex.collect { case (Var(n), i) => n -> mkObj(x, List(TupleField(i, Some(elems.size)))) }.toMap
      case _ =>
        Map.empty[Name, Obj]
    }

    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]
    for (clause <- c.clauses) {
      if (linearVars(clause)) {
        val pat = clause.pats.head
        val (patPos, patNeg) =
          pat match {
            case PatVar(`x`) => (None, None)
            case _           => patProps(x, Nil, pat, env).unzip
          }
        val aMap = aliases(x, Nil, pat, env).toMap
        val (testPos, testNeg) = guardsProps(clause.guards, aMap)
        val clauseProps = combine(patPos.toList ++ testPos, propsAcc)
        val clauseEnv = batchSelect(env1, clauseProps, aMap ++ eMap)
        clauseEnvs.addOne(clauseEnv)
        propsAcc = {
          val allNeg = patNeg.toList ++ testNeg
          allNeg match {
            case Nil =>
              propsAcc
            case h :: Nil =>
              propsAcc :+ h
            case zz =>
              propsAcc :+ or(zz)
          }
        }
      } else {
        clauseEnvs.addOne(env)
      }
    }
    clauseEnvs.toList
  }

  def clausesEnvs(clauses: List[Clause], argTys: List[Type], env: Env): List[Env] = {
    val accumulateNegProps = isEnabled(clauses)
    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]

    val vars = argTys.map(_ => genVar())
    val env1 = env ++ vars.zip(argTys).toMap

    var clausesWithAliases = clauses.map { clause =>
      var aMap: AMap = Map.empty
      for ((x, pat) <- vars.zip(clause.pats)) {
        aMap = aMap ++ aliases(x, Nil, pat, env).toMap
      }
      (clause, aMap)
    }
    while (clausesWithAliases.nonEmpty) {
      val (clause, aMap) = clausesWithAliases.head
      clausesWithAliases = clausesWithAliases.tail
      if (linearVars(clause)) {
        val pats = clause.pats
        val allPos = ListBuffer.empty[Prop]
        val allNeg = ListBuffer.empty[Prop]
        for ((x, pat) <- vars.zip(pats)) {
          val (patPos, patNeg) = patProps(x, Nil, pat, env).unzip
          patPos.foreach(allPos.addOne)
          patNeg.foreach(allNeg.addOne)
        }
        val (testPos, testNeg) = guardsProps(clause.guards, aMap)
        val clauseProps =
          if (accumulateNegProps) combine((allPos ++ testPos).toList, propsAcc)
          else (allPos ++ testPos).toList
        val clauseEnv = batchSelect(env1, clauseProps, aMap)
        clauseEnvs.addOne(clauseEnv)
        propsAcc = {
          val allNeg1 =
            if (!ignoreNumberRefinement(clause, aMap, clausesWithAliases)) allNeg.toList ++ testNeg
            else allNeg.toList
          allNeg1 match {
            case Nil =>
              propsAcc
            case h :: Nil =>
              propsAcc :+ h
            case zz =>
              propsAcc :+ or(zz)
          }
        }
      } else {
        clauseEnvs.addOne(env)
      }
    }
    clauseEnvs.toList
  }

  def testEnv(test: Test, env: Env, result: Boolean): Env = {
    val (testPos, testNeg) = testProps(test, Map.empty)
    val relevantProp = if (result) testPos else testNeg
    batchSelect(env, List(relevantProp), Map.empty)
  }

  /** Combines the propositions from `posProps` and `props` into a single list,
    * taking the positive statements from `posProps` to filter out redundant
    * negative statements from `props`.
    */
  private def combine(posProps: List[Prop], props: List[Prop]): List[Prop] = {
    def collectPos(p: Prop): List[(Obj, Type)] = {
      p match {
        case Pos(obj, t) => List((obj, t))
        case And(ps)     => ps.flatMap(collectPos)
        case _           => List()
      }
    }
    val posTypes: List[(Obj, Type)] = posProps.flatMap(collectPos)
    def isRedundant(p: Prop): Boolean = {
      p match {
        case Neg(obj, t) =>
          posTypes.exists { case (obj2, t2) => obj == obj2 && overlap(t, t2).contains(false) }
        case Or(ps) =>
          ps.exists(isRedundant)
        case _ =>
          false
      }
    }
    props.filter(!isRedundant(_)) ++ posProps
  }

  private def aliases(x: String, path: Path, pat: Pat, env: Env): List[(Name, Obj)] =
    pat match {
      case PatVar(v) if !env.contains(v) =>
        val obj = mkObj(x, path)
        List(v -> obj)
      case PatTuple(elems) =>
        val arity = elems.size
        elems.zipWithIndex.flatMap { case (elem, i) =>
          val pathI = path ++ List(TupleField(i, Some(arity)))
          aliases(x, pathI, elem, env)
        }
      case PatRecord(recName, fields, gen) =>
        val fieldsAliases =
          fields.flatMap(field => aliases(x, path ++ List(RecordField(field.name, recName)), field.pat, env))
        gen match {
          case None => fieldsAliases
          case Some(genPat) =>
            val recDecl = util.getRecord(module, recName)
            recDecl match {
              case None => fieldsAliases
              case Some(recDecl) =>
                val genAliases =
                  recDecl.fields
                    .filter(fDecl => !fields.exists(f => f.name == fDecl._1))
                    .flatMap(fDecl => aliases(x, path ++ List(RecordField(fDecl._1, recName)), genPat, env))
                    .toList
                genAliases ++ fieldsAliases
            }
        }
      case PatMatch(pat1, pat2) =>
        aliases(x, path, pat1, env) ++ aliases(x, path, pat2, env)
      case PatMap(pats) =>
        pats.flatMap { case (patKey, patR) =>
          Key.fromTest(patKey).map { key =>
            val pathI = path ++ List(MapField(key))
            aliases(x, pathI, patR, env)
          }
        }.flatten
      case PatCons(hpat, tpat) =>
        aliases(x, path ++ List(ListHead), hpat, env) ++ aliases(x, path ++ List(ListTail), tpat, env)
      case _ =>
        Nil
    }

  private def guardsProps(guards: List[Guard], aMap: Map[Name, Obj]): (Option[Prop], Option[Prop]) =
    // the same as connecting via OR
    if (guards.isEmpty) (None, None)
    else {
      val (pos, neg) = guards.map(guardProp(_, aMap)).unzip
      (Some(or(pos)), Some(and(neg)))
    }

  private def guardProp(guard: Guard, aMap: Map[Name, Obj]): (Prop, Prop) = {
    // the same as connecting via AND
    val (pos, neg) = guard.tests.map(testProps(_, aMap)).unzip
    (and(pos), or(neg))
  }

  private def testObj(test: Test, aMap: Map[Name, Obj]): Option[Obj] = {
    test match {
      case TestVar(v) =>
        Some(aMap.getOrElse(v, VarObj(v)))
      case TestRecordSelect(rec, recName, fieldName) =>
        testObj(rec, aMap).map(FieldObj(RecordField(fieldName, recName), _))
      case TestCall(Id("hd", 1), List(arg)) =>
        testObj(arg, aMap).map(FieldObj(ListHead, _))
      case TestCall(Id("element", 2), List(TestNumber(Some(index)), arg)) =>
        testObj(arg, aMap).map(FieldObj(TupleField(index.toInt, None), _))
      case _ =>
        None
    }
  }

  private def cmpTypes(test: Test): (Option[Type], Option[Type]) = {
    def unzipOpt(tys: List[Option[Type]]): Option[List[Type]] = {
      tys
        .foldLeft(Option(List.empty[Type])) {
          case (None, _) | (_, None) => None
          case (Some(l), Some(ty))   => Some(ty :: l)
        }
        .map(_.reverse)
    }
    test match {
      case TestAtom(s)     => (Some(AtomLitType(s)), Some(AtomLitType(s)))
      case TestBinaryLit() => (Some(BinaryType), None)
      case TestNumber(_)   => (Some(NumberType), None)
      case TestTuple(tests) =>
        val (pos, neg) = tests.map(cmpTypes).unzip
        (unzipOpt(pos).map(TupleType), unzipOpt(neg).map(TupleType))
      case _ => (None, None)
    }
  }

  private def cmpProps(obj: Obj, test: Test): (Prop, Prop) = {
    val (posTy, negTy) = cmpTypes(test)
    val pos = posTy.map(Pos(obj, _)).getOrElse(Unknown)
    val neg = negTy.map(Neg(obj, _)).getOrElse(Unknown)
    (pos, neg)
  }

  private def testProps(test: Test, aMap: Map[Name, Obj]): (Prop, Prop) = {
    test match {
      case TestCall(Id(pred, 1), List(arg)) if unary_predicates.isDefinedAt(pred) =>
        val tp = unary_predicates(pred)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_function", 2), List(arg, TestNumber(Some(arity)))) =>
        val tp = FunType(Nil, List.fill(arity.intValue)(AnyType), AnyType)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_record", 2 | 3), arg :: TestAtom(recName) :: _) =>
        val tp = RecordType(recName)(module)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestUnOp("not", test) =>
        testProps(test, aMap).swap
      case TestBinOp("and" | "andalso", test1, test2) =>
        val (pos1, neg1) = testProps(test1, aMap)
        val (pos2, neg2) = testProps(test2, aMap)
        (and(List(pos1, pos2)), or(List(neg1, neg2)))
      case TestBinOp("or" | "orelse", test1, test2) =>
        val (pos1, neg1) = testProps(test1, aMap)
        val (pos2, neg2) = testProps(test2, aMap)
        (or(List(pos1, pos2)), and(List(neg1, neg2)))
      case TestBinOp("==" | "=:=", lhs, cmp) =>
        testObj(lhs, aMap)
          .map(cmpProps(_, cmp))
          .getOrElse((Unknown, Unknown))
      case TestBinOp("=/=" | "/=", lhs, cmp) =>
        testObj(lhs, aMap)
          .map(cmpProps(_, cmp))
          .getOrElse((Unknown, Unknown))
          .swap
      case _ =>
        (Unknown, Unknown)
    }
  }

  private def patProps(x: String, path: Path, pat: Pat, env: Env): Option[(Prop, Prop)] = {
    pat match {
      case PatWild() =>
        None
      case PatVar(v) =>
        env.get(v) map { _ => (Unknown, Unknown) }
      case PatAtom(s) =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, AtomLitType(s))
        val neg = Neg(obj, AtomLitType(s))
        Some(pos, neg)
      case PatNumber() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, NumberType)
        Some(pos, Unknown)
      case PatInt() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, NumberType)
        Some(pos, Unknown)
      case PatTuple(elems) =>
        val arity = elems.size
        val obj = mkObj(x, path)
        val posThis = Pos(obj, TupleType(List.fill(arity)(AnyType)))
        val negThis = Neg(obj, TupleType(List.fill(arity)(AnyType)))
        val (posThat, negThat) = elems.zipWithIndex.flatMap { case (elem, i) =>
          patProps(x, path :+ TupleField(i, Some(arity)), elem, env)
        }.unzip
        val pos = and(posThis :: posThat)
        val neg =
          if (negThat.isEmpty) negThis
          else or(List(negThis, and(List(posThis, or(negThat)))))
        Some(pos, neg)
      case PatRecord(recName, fields, gen) =>
        val obj = mkObj(x, path)
        val posThis = Pos(obj, RecordType(recName)(module))
        val negThis = Neg(obj, RecordType(recName)(module))
        val (posNamed, negNamed) =
          fields.flatMap(field => patProps(x, path :+ RecordField(field.name, recName), field.pat, env)).unzip
        val (posThat, negThat) = gen match {
          case None => (posNamed, negNamed)
          case Some(genPat) =>
            val recDecl = util.getRecord(module, recName)
            recDecl match {
              case None => (posNamed, negNamed)
              case Some(recDecl) =>
                val (posGen, negGen) =
                  recDecl.fields
                    .filter(fDecl => !fields.exists(f => f.name == fDecl._1))
                    .flatMap(fDecl => patProps(x, path :+ RecordField(fDecl._1, recName), genPat, env))
                    .toList
                    .unzip
                (posNamed ++ posGen, negNamed ++ negGen)
            }
        }
        val pos = and(posThis :: posThat)
        val neg =
          if (negThat.isEmpty) negThis
          else or(List(negThis, and(List(posThis, or(negThat)))))
        Some(pos, neg)
      case PatMatch(PatVar(alias), pat1) =>
        env.get(alias) match {
          case Some(_) =>
            Some(Unknown, Unknown)
          case None =>
            patProps(x, path, pat1, env)
        }
      case PatMatch(pat1, PatVar(alias)) =>
        env.get(alias) match {
          case Some(_) =>
            Some(Unknown, Unknown)
          case None =>
            patProps(x, path, pat1, env)
        }
      case PatMap(pats) =>
        val obj = mkObj(x, path)
        val posThis = Pos(obj, MapType(Map(), AnyType, AnyType))
        val negThis = Neg(obj, MapType(Map(), AnyType, AnyType))
        val fields = pats.map { case (patK, patV) =>
          Key.fromTest(patK) match {
            case Some(key) => (key, patV)
            case None      => return Some(posThis, Unknown)
          }
        }
        val (posThat, negThat) = fields.flatMap { case (field, pat) =>
          patProps(x, path :+ MapField(field), pat, env)
        }.unzip
        val (posFields, negFields) = fields.map { case (field, _) =>
          val objField = mkObj(x, path :+ MapField(field))
          (Pos(objField, AnyType), Neg(objField, AnyType))
        }.unzip
        val pos = and(posThis :: posFields ::: posThat)
        val neg =
          if (negThat.isEmpty && negFields.isEmpty) negThis
          else or(List(negThis, and(List(posThis, or(negThat ::: negFields)))))
        Some(pos, neg)
      case PatNil() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, NilType)
        val neg = Neg(obj, NilType)
        Some(pos, neg)
      case PatCons(hpat, tpat) =>
        val obj = mkObj(x, path)
        val posThis = And(List(Pos(obj, ListType(AnyType)), Neg(obj, NilType)))
        val negThis = Or(List(Neg(obj, ListType(AnyType)), Pos(obj, NilType)))
        val (posThat, negThat) =
          List(patProps(x, path :+ ListHead, hpat, env), patProps(x, path :+ ListTail, tpat, env)).flatten.unzip
        val pos = And(posThis :: posThat)
        val neg = Or(List(negThis, And(List(posThis, Or(negThat)))))
        Some(pos, neg)
      case PatBinary(_) =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, BinaryType)
        val neg = Unknown
        Some(pos, neg)
      case _ =>
        Some(Unknown, Unknown)
    }
  }

  private def simpleOverlap(t1: Type, t2: Type): Option[Boolean] =
    (kind(t1), kind(t2)) match {
      case (Some(k1), Some(k2)) =>
        Some(k1 == k2)
      case (_, _) =>
        if (subtype.subType(t1, t2) || subtype.subType(t2, t1))
          Some(true)
        else
          None
    }

  private def overlap(t1: Type, t2: Type): Option[Boolean] =
    (t1, t2) match {
      case (_, _) if t1 == t2 =>
        Some(true)
      case (AnyType, _) =>
        Some(true)
      case (_, AnyType) =>
        Some(true)
      case (NoneType, _) =>
        Some(false)

      case (DynamicType, _) =>
        Some(true)
      case (_, DynamicType) =>
        Some(true)

      case (BoundedDynamicType(bound), _) =>
        overlap(bound, t2)
      case (_, BoundedDynamicType(bound)) =>
        overlap(t1, bound)

      case (VarType(_), _) =>
        Some(true)

      // Unions
      case (UnionType(ts), t) =>
        val subs = ts.map(overlap(_, t))
        if (subs.exists(_.isTrue))
          Some(true)
        else if (subs.forall(_.isFalse))
          Some(false)
        else
          None
      case (t, UnionType(ts)) =>
        val subs = ts.map(overlap(t, _))
        if (subs.exists(_.isTrue))
          Some(true)
        else if (subs.forall(_.isFalse))
          Some(false)
        else
          None

      case (AtomLitType(l1), AtomLitType(l2)) =>
        Some(l1 == l2)

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        overlap(body, t2)
      case (OpaqueType(_, _), _) =>
        None
      // t2 is generated from "predicates" - they are always without aliases
      case (_, RemoteType(_, _)) =>
        throw new IllegalStateException(t2.toString)
      case (_, OpaqueType(_, _)) =>
        throw new IllegalStateException(t2.toString)

      // funs
      case (FunType(_, ins1, _), FunType(_, ins2, _)) =>
        if (ins1.size != ins2.size)
          Some(false)
        else
          None
      case (FunType(_, _, _), AnyFunType) =>
        Some(true)
      case (AnyFunType, FunType(_, _, _)) =>
        Some(true)
      case (AnyArityFunType(_), AnyFunType) =>
        Some(true)
      case (AnyFunType, AnyArityFunType(_)) =>
        Some(true)
      case (AnyArityFunType(_), FunType(_, _, _)) =>
        None
      case (FunType(_, _, _), AnyArityFunType(_)) =>
        None
      case (FunType(_, _, _), _) =>
        Some(false)
      case (_, FunType(_, _, _)) =>
        Some(false)
      case (AnyFunType, _) =>
        Some(false)
      case (_, AnyFunType) =>
        Some(false)

      // tuples and records
      case (TupleType(ts1), TupleType(ts2)) =>
        if (ts1.size != ts2.size) Some(false)
        else {
          val overlaps = ts1.lazyZip(ts2).map(overlap)
          if (overlaps.exists(_.isFalse))
            Some(false)
          else if (overlaps.forall(_.isTrue))
            Some(true)
          else
            None
        }
      case (TupleType(_), AnyTupleType) =>
        Some(true)
      case (AnyTupleType, TupleType(_)) =>
        Some(true)
      case (RecordType(_), AnyTupleType) =>
        Some(true)
      case (RefinedRecordType(_, _), AnyTupleType) =>
        Some(true)
      case (AnyTupleType, RecordType(_)) =>
        Some(true)
      case (RecordType(n1), RecordType(n2)) =>
        Some(n1 == n2)
      case (RefinedRecordType(t, _), RecordType(n)) =>
        Some(n == t.name)
      case (RecordType(name), TupleType(elems)) =>
        elems match {
          case Nil =>
            Some(false)
          case h :: _ =>
            overlap(AtomLitType(name), h)
        }
      case (TupleType(elems), RecordType(name)) =>
        elems match {
          case Nil =>
            Some(false)
          case h :: _ =>
            overlap(h, AtomLitType(name))
        }
      case (RefinedRecordType(t, _), TupleType(elems)) =>
        elems match {
          case Nil =>
            Some(false)
          case h :: _ =>
            overlap(AtomLitType(t.name), h)
        }
      case (TupleType(_), _) =>
        Some(false)
      case (_, TupleType(_)) =>
        Some(false)
      case (AnyTupleType, _) =>
        Some(false)
      case (_, AnyTupleType) =>
        Some(false)

      case (_, RefinedRecordType(_, _)) =>
        // t2 comes from props
        throw new IllegalStateException(t2.toString)

      case (ListType(_) | NilType, ListType(_) | NilType) =>
        Some(true)
      case (ListType(_) | NilType, _) =>
        Some(false)
      case (_, ListType(_) | NilType) =>
        Some(false)

      case _ =>
        simpleOverlap(t1, t2)
    }

  private def restrict(t1: Type, t2: Type): Type = {
    (t1, t2) match {
      case (t, s) if overlap(t, s).isFalse =>
        NoneType
      case (t, s) if subtype.gradualSubType(t, s) =>
        t
      case (t, s) if subtype.gradualSubType(s, t) =>
        s
      case (UnionType(ts), s) =>
        UnionType(ts.map(restrict(_, s)))
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        restrict(body, t2)
      case (BoundedDynamicType(t), s) =>
        BoundedDynamicType(restrict(t, s))
      case (DynamicType, s) =>
        BoundedDynamicType(s)
      case (OpaqueType(_, _), _) =>
        t1
      case (_, _) =>
        t1
    }
  }

  def remove(t1: Type, t2: Type): Type =
    (t1, t2) match {
      case (t, s) if subtype.gradualSubType(t, s) =>
        NoneType
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        body match {
          case UnionType(ts) if (!pipelineContext.unlimitedRefinement) && (ts.size > 16) =>
            t1
          case _ =>
            remove(body, t2)
        }
      case (OpaqueType(_, _), _) =>
        t1
      case (UnionType(ts), s) =>
        UnionType(ts.map(remove(_, s)))
      case (BoundedDynamicType(t), s) =>
        BoundedDynamicType(remove(t, s))
      case (t, _) =>
        t
    }

  private def TupleType_*(elems: List[Type]): Type =
    if (elems.exists(subtype.isNoneType))
      NoneType
    else
      TupleType(elems)

  private def MapType_*(props: Map[Types.Key, Types.Prop], kTy: Type, vTy: Type): Type = {
    val hasPropEmpty = props.values.exists { case Prop(req, tp) => req && subtype.isNoneType(tp) }
    val propsNonEmpty = props.filter { case (_, Prop(req, tp)) => req || !subtype.isNoneType(tp) }
    if (hasPropEmpty)
      NoneType
    else
      MapType(propsNonEmpty, kTy, vTy)
  }

  private def refineRecord(t: Type, field: String, refined: Type): Type = {
    if (subtype.isNoneType(refined)) {
      NoneType
    } else {
      t match {
        case rt: RefinedRecordType =>
          RefinedRecordType(rt.recType, rt.fields.updated(field, refined))
        case rt: RecordType =>
          RefinedRecordType(rt, Map(field -> refined))
        case _ => t
      }
    }
  }

  private def update(t: Type, path: Path, pol: Polarity, s: Type): Type =
    (t, path) match {
      case (_, Nil) =>
        pol match {
          case + => restrict(t, s)
          case - => remove(t, s)
        }
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        update(body, path, pol, s)
      case (UnionType(ts), _) =>
        UnionType(ts.map(update(_, path, pol, s)))
      case (BoundedDynamicType(t), _) =>
        BoundedDynamicType(update(t, path, pol, s))
      case (TupleType(ts), TupleField(pos, Some(arity)) :: path) if ts.size == arity =>
        val t = ts(pos)
        val t1 = update(t, path, pol, s)
        TupleType_*(ts.updated(pos, t1))
      case (rt: RecordType, RecordField(fieldName, recName) :: path) if rt.name == recName =>
        util.getRecord(rt.module, rt.name) match {
          case Some(recDecl) =>
            val t = recDecl.fields(fieldName).tp
            val t1 = update(t, path, pol, s)
            refineRecord(rt, fieldName, t1)
          case _ => rt
        }
      case (rt: RecordType, TupleField(_, Some(arity)) :: _) =>
        util.getRecord(rt.module, rt.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val rTy = narrow.asTupleType(rt, arity).head
            update(rTy, path, pol, s)
          case _ => rt
        }
      case (rt: RefinedRecordType, RecordField(fieldName, recName) :: path) if rt.recType.name == recName =>
        if (rt.fields.contains(fieldName)) {
          val t = rt.fields(fieldName)
          val t1 = update(t, path, pol, s)
          refineRecord(rt, fieldName, t1)
        } else {
          util.getRecord(rt.recType.module, rt.recType.name) match {
            case Some(recDecl) =>
              val t = recDecl.fields(fieldName).tp
              val t1 = update(t, path, pol, s)
              refineRecord(rt, fieldName, t1)
            case None => rt
          }
        }
      case (rt: RefinedRecordType, TupleField(_, Some(arity)) :: _) =>
        util.getRecord(rt.recType.module, rt.recType.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val rTy = narrow.asTupleType(rt, arity).head
            update(rTy, path, pol, s)
          case _ => rt
        }
      case (MapType(props, kTy, vTy), MapField(field) :: path) =>
        if (props.contains(field) || (subtype.subType(Key.asType(field), kTy) && pol == +)) {
          val refinedProps = props.updatedWith(field) {
            case Some(Prop(req, tp)) => Some(Prop((pol == +) || req, update(tp, path, pol, s)))
            case None                => Some(Prop(req = true, update(vTy, path, pol, s)))
          }
          MapType_*(refinedProps, kTy, vTy)
        } else {
          pol match {
            case + => NoneType
            case - => t
          }
        }
      case (ListType(lt), ListHead :: path) =>
        if (subtype.isNoneType(update(lt, path, pol, s)))
          NoneType
        else
          ListType(lt)
      case (ListType(lt), ListTail :: path) =>
        if (subtype.isNoneType(update(ListType(lt), path, pol, s)))
          NoneType
        else
          ListType(lt)
      case (_, TupleField(_, None) :: _) =>
        AnyTupleType
      case (_, _) =>
        t
    }

  private def batchSelect(typeEnv: Env, propEnv: PropEnv, aMap: AMap): Env = {
    val refinedEnvs = applyProps(propEnv, List(typeEnv))
    var result: Env = Map.empty
    val names = typeEnv.keySet ++ aMap.keySet
    for (name <- names) {
      val ts = aMap.get(name) match {
        case Some(obj) =>
          val id = objId(obj)
          val path = objPath(obj)
          refinedEnvs.map(chooseType(_, id, typeEnv)).map(typePathRef(_, path))
        case None =>
          refinedEnvs.map(chooseType(_, name, typeEnv))
      }
      val t = ts match {
        case List(t1) => t1
        case _        => subtype.join(ts)
      }
      result += name -> t
    }
    result
  }

  private def envSubtype(env1: Env, env2: Env): Boolean =
    env1.forall { case (k1, t1) => env2.contains(k1) && subtype.gradualSubType(t1, env2(k1)) }

  /** Removes redundant environments from a list
    * by keeping only the less precise ones for subtyping */
  private def keepBestEnvs(envs: List[Env]): List[Env] = {
    var acc: List[Env] = List()
    envs.foreach { env =>
      if (!acc.exists(envSubtype(env, _))) {
        acc = env :: acc.filter(!envSubtype(_, env))
      }
    }
    acc
  }

  private def applyProps(props: List[Prop], envs: List[Env]): List[Env] =
    props match {
      case Nil =>
        envs
      case False :: _ =>
        List()
      case True :: props =>
        applyProps(props, envs)
      case Unknown :: props =>
        applyProps(props, envs)
      case Pos(x, t) :: props =>
        applyProps(props, keepBestEnvs(envs.map(updateTypeEnv(_, +, x, t))))
      case Neg(x, t) :: props =>
        applyProps(props, keepBestEnvs(envs.map(updateTypeEnv(_, -, x, t))))
      case And(ps) :: props =>
        applyProps(ps ++ props, envs)
      case Or(ps) :: props =>
        val envs2 = applyProps(props, envs)
        keepBestEnvs(ps.flatMap((p: Prop) => applyProps(List(p), envs2)))
    }

  private def chooseType(typeEnv: Env, x: String, originalEnv: Env): Type = {
    // The second isNoneType check is there to reduce unwanted noise when none() is not introduced by refining
    if (typeEnv.exists { case (s, t) => subtype.isNoneType(t) && !subtype.isNoneType(originalEnv(s)) }) NoneType
    else typeEnv(x)
  }

  private def updateTypeEnv(typeEnv: Env, pol: Polarity, obj: Obj, t: Type): Env = {
    val x = objId(obj)
    typeEnv.get(x) match {
      case None =>
        typeEnv
      case Some(old) =>
        val s = update(old, objPath(obj), pol, t)
        typeEnv.updated(x, s)
    }
  }

  @tailrec
  private def objId(obj: Obj): String =
    obj match {
      case VarObj(v)      => v
      case FieldObj(_, o) => objId(o)
    }

  private def objPath(obj: Obj): Path =
    obj match {
      case VarObj(_) =>
        List.empty
      case FieldObj(field, obj) =>
        field :: objPath(obj)
    }

  private def typePathRef(t: Type, path: Path): Type =
    (t, path) match {
      case (NoneType, _) =>
        NoneType
      case (s, Nil) =>
        s
      case (DynamicType, TupleField(_, _) :: _) =>
        DynamicType
      case (DynamicType, RecordField(fieldName, recName) :: path1) =>
        util
          .getRecord(module, recName)
          .map(_.fields(fieldName).tp)
          .map(typePathRef(_, path1))
          .getOrElse(DynamicType)
      case (BoundedDynamicType(bound), _) =>
        BoundedDynamicType(typePathRef(bound, path))
      case (UnionType(ts), _) =>
        UnionType(ts.map(typePathRef(_, path)))
      case (TupleType(ts), TupleField(index, Some(arity)) :: path1) if ts.size == arity =>
        typePathRef(ts(index), path1)
      case (rTy: RecordType, RecordField(fieldName, recName) :: path1) if rTy.name == recName =>
        util
          .getRecord(rTy.module, rTy.name)
          .map(_.fields(fieldName).tp)
          .map(typePathRef(_, path1))
          .getOrElse(AnyType)
      case (rTy: RecordType, TupleField(index, Some(arity)) :: path1) =>
        util.getRecord(rTy.module, rTy.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val tuple = narrow.asTupleType(rTy, arity).head
            typePathRef(tuple.argTys(index), path1)
          case _ => AnyType
        }
      case (rTy: RefinedRecordType, RecordField(fieldName, recName) :: path1) if rTy.recType.name == recName =>
        if (rTy.fields.contains(fieldName)) {
          typePathRef(rTy.fields(fieldName), path1)
        } else {
          util
            .getRecord(rTy.recType.module, rTy.recType.name)
            .map(_.fields(fieldName).tp)
            .map(typePathRef(_, path1))
            .getOrElse(AnyType)
        }
      case (rTy: RefinedRecordType, TupleField(index, Some(arity)) :: path1) =>
        util.getRecord(rTy.recType.module, rTy.recType.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val tuple = narrow.asTupleType(rTy, arity).head
            typePathRef(tuple.argTys(index), path1)
          case _ => AnyType
        }
      case (MapType(props, _, vTy), MapField(field) :: path1) =>
        val ty = props
          .get(field)
          .map(_.tp)
          .getOrElse(vTy)
        typePathRef(ty, path1)
      case (RemoteType(rid, args), path) =>
        val body = util.getTypeDeclBody(rid, args)
        typePathRef(body, path)
      case (ListType(lt), ListHead :: path) =>
        typePathRef(lt, path)
      case (ListType(lt), ListTail :: path) =>
        typePathRef(ListType(lt), path)
      case _ =>
        AnyType
    }

  private def mkObj(v: String, path: Path): Obj =
    path match {
      case Nil =>
        VarObj(v)
      case field :: path =>
        FieldObj(field, mkObj(v, path))
    }
}
