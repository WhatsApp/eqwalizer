/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.ericsson.otp.erlang.{OtpErlangObject, OtpExternal, OtpInputStream, OtpOutputStream}
import com.whatsapp.eqwalizer.ast.Forms.TypeDecl
import com.whatsapp.eqwalizer.ast.{Id, RemoteId}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.io.EData
import com.whatsapp.eqwalizer.io.EData._
import com.whatsapp.eqwalizer.tc.Subst

import java.io.DataInputStream
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.Using

// Checks a dbg trace wrt specs and types.
// It analyses `call` and `return_from` elements of the trace.
// See https://www.erlang.org/doc/man/dbg.html for more details.
object DbgTraceCheck {
  // $COVERAGE-OFF$
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      Console.println("usage: com.whatsapp.eqwalizer.analyses.DbgTraceCheck <trace_file>|<trace_dir>")
      return
    }
    val traceObj = args.head
    if (Paths.get(traceObj).toFile.isDirectory)
      processTraceDirectory(traceObj)
    else
      processTraceFile(traceObj)
  }

  private def processTraceDirectory(traceDir: String): Unit =
    Paths.get(traceDir).toFile.listFiles((_, f) => f.endsWith(".dbg_trace")).foreach { file =>
      processTraceFile(file.getPath)
    }

  private def processTraceFile(traceFile: String): Unit = {
    val errorsFile = Paths.get(traceFile ++ ".errors")
    var index = 0
    val errBuffer = ListBuffer.empty[EObject]

    iterateTrace(Paths.get(traceFile)) { chunk =>
      for (error <- analyzeChunk(chunk)) {
        val elem = error match {
          case _: ResultError =>
            EAtom("res")
          case err: ArgumentError =>
            ETuple(List(EAtom("arg"), ELong(err.index)))
        }
        errBuffer.addOne(ETuple(List(ELong(index), elem)))
      }
      index += 1
    }

    val bytes = {
      val outs = new OtpOutputStream()
      outs.write(OtpExternal.versionTag)
      outs.write_any(toJava(EList(errBuffer.toList, None)))
      outs.toByteArray
    }

    Files.write(errorsFile, bytes)
  }
  // $COVERAGE-ON$

  sealed trait Error
  case class ResultError(module: String, id: Id, value: EObject) extends Error
  case class ArgumentError(module: String, id: Id, index: Int, value: EObject) extends Error

  def check(traceFile: String): List[Error] = {
    val result = ListBuffer.empty[Error]
    iterateTrace(Paths.get(traceFile)) { chunk => result.addAll(analyzeChunk(chunk)) }
    result.toList
  }

  private def analyzeChunk(chunk: OtpErlangObject): List[Error] =
    EData.fromJava(chunk) match {
      case ETuple(List(EAtom("trace"), EPid, EAtom("call"), ETuple(List(EAtom(module), EAtom(fun), eArgs)), _caller)) =>
        val Some(EList(args, None)) = asList(eArgs)
        val id = Id(fun, args.length)
        checkArgs(module, id, args)
      case ETuple(
            List(
              EAtom("trace"),
              EPid,
              EAtom("return_from"),
              ETuple(List(EAtom(module), EAtom(fun), ELong(arity))),
              eResult,
            )
          ) =>
        val id = Id(fun, arity.toInt)
        checkResult(module, id, eResult).toList
      case _ =>
        List.empty
    }

  private def asList(obj: EObject): Option[EList] =
    obj match {
      case l: EList =>
        Some(l)
      case s: EString =>
        Some(EList(s.str.toCharArray.toList.map(c => ELong(c)), None))
      case _ =>
        None
    }

  private def checkArgs(module: String, id: Id, args: List[EObject]): List[ArgumentError] =
    DbApi.getRawSpec(module, id) match {
      case Some(spec) =>
        for {
          ((arg, i), ty) <- args.zipWithIndex.zip(spec.ty.argTys)
          if !checkValue(arg, ty)
        } yield ArgumentError(module, id, i, arg)
      case None =>
        // not checking arguments of overloaded specs yet
        List.empty
    }

  private def checkResult(module: String, id: Id, result: EObject): Option[ResultError] =
    DbApi.getRawSpec(module, id) match {
      case Some(spec) =>
        if (checkValue(result, spec.ty.resTy))
          None
        else
          Some(ResultError(module, id, result))
      case None =>
        DbApi.getRawOverloadedSpec(module, id) match {
          case None =>
            None
          case Some(spec) =>
            for (resTy <- spec.tys.map(_.resTy)) {
              if (checkValue(result, resTy))
                return None
            }
            Some(ResultError(module, id, result))
        }
    }

  private def checkValue(value: EObject, ty: Type): Boolean =
    checkTy(value, ty)

  private def checkTy(value: EObject, ty: Type): Boolean =
    ty match {
      case NoneType =>
        false
      case AnyType =>
        true
      case DynamicType =>
        true
      case VarType(_) =>
        true
      case AtomType =>
        value match {
          case EAtom(_) =>
            true
          case _ =>
            false
        }
      case AtomLitType(atom) =>
        value match {
          case EAtom(a) =>
            a == atom
          case _ =>
            false
        }
      case AnyFunType =>
        value match {
          case EFun =>
            true
          case _ =>
            false
        }
      case FunType(_, _, _) =>
        value match {
          case EFun =>
            true
          case _ =>
            false
        }
      case AnyArityFunType(_) =>
        value match {
          case EFun =>
            true
          case _ =>
            false
        }
      case AnyTupleType =>
        value match {
          case ETuple(_) =>
            true
          case _ =>
            false
        }
      case TupleType(eTys) =>
        value match {
          case ETuple(elems) if elems.size == eTys.size =>
            elems.lazyZip(eTys).forall(checkTy)
          case _ =>
            false
        }
      case NilType =>
        value match {
          case EList(Nil, None) =>
            true
          case _ =>
            false
        }
      case BinaryType =>
        value match {
          case EBitStr(_, _) =>
            true
          case _ =>
            false
        }
      case PidType =>
        value match {
          case EPid =>
            true
          case _ =>
            false
        }
      case PortType =>
        value match {
          case EPort =>
            true
          case _ =>
            false
        }
      case ReferenceType =>
        value match {
          case ERef =>
            true
          case _ =>
            false
        }
      case NumberType =>
        value match {
          case EDouble(_) =>
            true
          case ELong(_) =>
            true
          case _ =>
            false
        }
      case UnionType(tys) =>
        tys.exists(checkTy(value, _))
      case RemoteType(id, argTys) =>
        val Some(body) = getTypeDeclBody(id, argTys)
        checkTy(value, body)
      case OpaqueType(id, argTys) =>
        val Some(body) = getTypeDeclBody(id, argTys)
        checkTy(value, body)
      case ListType(t) =>
        value match {
          case EList(elems, _) =>
            elems.forall(checkTy(_, t))
          case EString(str) =>
            if (str.isEmpty)
              checkTy(EList(List.empty, None), t)
            else
              checkTy(ELong(0), t)
          case _ =>
            false
        }
      case rt @ RecordType(name) =>
        val Some(rec) = DbApi.getRawRecord(rt.module, name)
        val tupleType = TupleType(AtomLitType(name) :: rec.fields.map(_.tp.getOrElse(AnyType)))
        checkTy(value, tupleType)
      case RefinedRecordType(rt, fields) =>
        val Some(rec) = DbApi.getRawRecord(rt.module, rt.name)
        val tupleType = TupleType(AtomLitType(rt.name) :: rec.fields.map(_.tp.getOrElse(AnyType)))
        checkTy(value, tupleType)
      case DictMap(kTy, vTy) =>
        value match {
          case EMap(entries) =>
            entries.forall { case (k, v) => checkTy(k, kTy) && checkTy(v, vTy) }
          case _ =>
            false
        }
      case ShapeMap(props) =>
        value match {
          case EMap(entries) =>
            val map = props.map(p => (p.key, p.tp)).toMap
            var requiredProps = props.collect { case ReqProp(k, _) => k }.toSet
            for { (k, v) <- entries } {
              k match {
                case EAtom(atom) =>
                  map.get(atom) match {
                    case Some(vt) =>
                      requiredProps = requiredProps - atom
                      if (!checkTy(v, vt))
                        return false
                    case None =>
                      return false
                  }
                case _ =>
                  return false
              }
            }
            requiredProps.isEmpty
          case _ =>
            false
        }
    }

  def getTypeDeclBody(remoteId: RemoteId, args: List[Type]): Option[Type] = {
    val id = Id(remoteId.name, remoteId.arity)

    def applyType(decl: TypeDecl): Type = {
      val subst = decl.params.zip(args).map { case (VarType(n), ty) => n -> ty }.toMap
      Subst.subst(subst, decl.body)
    }

    DbApi.getRawType(remoteId.module, id) match {
      case Some(value) =>
        Some(applyType(value))
      case None =>
        DbApi.getRawOpaque(remoteId.module, id) match {
          case Some(value) =>
            Some(applyType(value))
          case None =>
            None
        }
    }
  }

  private def iterateTrace(path: Path)(f: OtpErlangObject => Unit): Unit =
    Using(new DataInputStream(Files.newInputStream(path))) { input =>
      while (input.available() > 0) {
        val tag = input.readByte()
        if (tag != 0)
          throw new IllegalArgumentException("Incomplete trace")
        val binarySize = input.readInt()
        val binary = new Array[Byte](binarySize)
        input.readFully(binary)
        val chunk = new OtpInputStream(binary).read_any()
        f(chunk)
      }
    }
}
