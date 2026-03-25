/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.io

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToStream}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.whatsapp.eqwalizer.ast.{Id, Pos}
import com.whatsapp.eqwalizer.ast.Types.Type
import com.whatsapp.eqwalizer.ast.Exprs.ExtType
import com.whatsapp.eqwalizer.util.ELPDiagnostics

object Ipc {
  case object Terminated extends Exception
  private case class GotNull() extends Exception

  enum ASTFormat {
    case ConvertedForms, TransitiveStub
  }

  def getAstBytes(module: String, kind: ASTFormat): Option[Array[Byte]] = {
    send(Request.GetAstBytes(module, kind))
    receive() match {
      case Right(Reply.GetAstBytesReply(len)) if len == 0 =>
        println()
        Console.out.flush()
        None
      case Right(Reply.GetAstBytesReply(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for $module but got $read")
        Some(buf)
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getAstBytes] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        // Happens when the client panics, such as ELP bug T111364923.
        // This error will only show up in logging, not really user-facing
        Console.err.println(s"eqWAlizer [getAstBytes] GotNull for module '$module', kind '$kind'")
        throw Terminated
    }
  }

  def getTypeDecl(module: String, id: Id): Option[Array[Byte]] = {
    send(Request.GetTypeDecl(module, id))
    receive() match
      case Right(Reply.GetTypeDeclReply(len)) =>
        if (len == 0) {
          println()
          Console.out.flush()
          None
        } else {
          println()
          Console.out.flush()
          val buf = new Array[Byte](len)
          val read = readNBytes(System.in, buf, 0, len)
          assert(read == len, s"expected $len but got $read")
          Some(buf)
        }
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getTypeDecl] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getTypeDecl] GotNull for module '$module', id '$id'")
        throw Terminated
  }

  def getRecDecl(module: String, id: String): Option[Array[Byte]] = {
    send(Request.GetRecDecl(module, id))
    receive() match
      case Right(Reply.GetRecDeclReply(len)) =>
        if (len == 0) {
          println()
          Console.out.flush()
          None
        } else {
          println()
          Console.out.flush()
          val buf = new Array[Byte](len)
          val read = readNBytes(System.in, buf, 0, len)
          assert(read == len, s"expected $len but got $read")
          Some(buf)
        }
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getRecDecl] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getRecDecl] GotNull for module '$module', id '$id'")
        throw Terminated
  }

  def getFunSpec(module: String, id: Id): Option[Array[Byte]] = {
    send(Request.GetFunSpec(module, id))
    receive() match
      case Right(Reply.GetFunSpecReply(len)) =>
        if (len == 0) {
          println()
          Console.out.flush()
          None
        } else {
          println()
          Console.out.flush()
          val buf = new Array[Byte](len)
          val read = readNBytes(System.in, buf, 0, len)
          assert(read == len, s"expected $len but got $read")
          Some(buf)
        }
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getFunSpec] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getFunSpec] GotNull for module '$module', id '$id'")
        throw Terminated
  }

  def getOverloadedFunSpec(module: String, id: Id): Option[Array[Byte]] = {
    send(Request.GetOverloadedFunSpec(module, id))
    receive() match
      case Right(Reply.GetOverloadedFunSpecReply(len)) =>
        if (len == 0) {
          println()
          Console.out.flush()
          None
        } else {
          println()
          Console.out.flush()
          val buf = new Array[Byte](len)
          val read = readNBytes(System.in, buf, 0, len)
          assert(read == len, s"expected $len but got $read")
          Some(buf)
        }
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getOverloadedFunSpec] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getOverloadedFunSpec] GotNull for module '$module', id '$id'")
        throw Terminated
  }

  def getCallbacks(module: String): Option[Array[Byte]] = {
    send(Request.GetCallbacks(module))
    receive() match
      case Right(Reply.GetCallbacksReply(len)) =>
        if (len == 0) {
          println()
          Console.out.flush()
          None
        } else {
          println()
          Console.out.flush()
          val buf = new Array[Byte](len)
          val read = readNBytes(System.in, buf, 0, len)
          assert(read == len, s"expected $len but got $read")
          Some(buf)
        }
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getCallbacks] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getCallbacks] GotNull for module '$module'")
        throw Terminated
  }

  def sendDone(diagnostics: Map[String, List[ELPDiagnostics.Error]], typeInfo: Map[String, List[(Pos, Type)]]): Unit =
    send(Request.Done(diagnostics, typeInfo))

  def sendEqwalizingStart(module: String): Unit =
    send(Request.EqwalizingStart(module))

  def sendEqwalizingDone(module: String): Unit =
    send(Request.EqwalizingDone(module))

  def validateType(ty: ExtType): (Boolean, Array[Byte]) = {
    send(Request.ValidateType(ty))
    receive() match {
      case Right(Reply.ValidatedType(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for validated type but got $read")
        (true, buf)
      case Right(Reply.InvalidType(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for validated type but got $read")
        (false, buf)
      case Right(Reply.CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [validateType] received bad reply from ELP when validating type: $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [validateType] GotNull for type '$ty'")
        throw Terminated
    }
  }

  private def send(req: Request): Unit = {
    writeToStream(req, Console.out)(requestCodec)
    Console.out.println()
    Console.flush()
  }

  def shouldEqwalize(module: String): Boolean = {
    send(Request.EnteringModule(module))
    receive() match {
      case Right(Reply.ELPEnteringModule) =>
        true
      case Right(Reply.ELPExitingModule) =>
        false
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [shouldEqwalize] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [shouldEqwalize] GotNull for module '$module'")
        throw Terminated
    }
  }

  def finishEqwalization(
      diagnostics: Map[String, List[ELPDiagnostics.Error]],
      deps: List[String],
      typeInfo: Map[String, List[(Pos, Type)]],
  ): Unit = {
    send(Request.Dependencies(deps))
    sendDone(diagnostics, typeInfo)
    receive() match {
      case Right(Reply.ELPExitingModule) =>
        ()
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [finishEqwalization] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [finishEqwalization] GotNull")
        throw Terminated
    }
  }

  private def receive(): Either[GotNull, Reply] = {
    val str = Console.in.readLine()
    if (str == null) {
      Left(GotNull())
    } else {
      Right(readFromString[Reply](str)(replyCodec))
    }
  }

  // copy/pasted from java.io.InputStream.readNBytes and then Scalafied.
  // replace with stream.readNBytes in T111884043
  private def readNBytes(stream: java.io.InputStream, b: Array[Byte], off: Int, len: Int): Int = {
    var n = 0

    while (n < len) {
      val count = stream.read(b, off + n, len - n)
      if (count < 0)
        return n
      n += count
    }
    n
  }

  enum Request {
    case EnteringModule(module: String)
    case ExitingModule(module: String) extends Request
    case GetAstBytes(module: String, format: ASTFormat)
    case EqwalizingStart(module: String)
    case EqwalizingDone(module: String)
    case Dependencies(modules: List[String])
    case Done(
        diagnostics: Map[String, List[ELPDiagnostics.Error]],
        typeInfo: Map[String, List[(Pos, Type)]],
    )
    case ValidateType(ty: ExtType)
    case GetTypeDecl(module: String, id: Id)
    case GetRecDecl(module: String, id: String)
    case GetFunSpec(module: String, id: Id)
    case GetOverloadedFunSpec(module: String, id: Id)
    case GetCallbacks(module: String)
  }

  enum Reply {
    case ELPEnteringModule
    case ELPExitingModule

    /**
     * Variants with `len` are non-JSON part of the protocol.
     * After receiving these messages, eqWAlizer prints a newline to stdout
     * and then reads `len` or `typeBytesLen` bytes from stdin
     */
    case GetAstBytesReply(len: Int)
    case ValidatedType(len: Int)
    case InvalidType(len: Int)
    case GetTypeDeclReply(len: Int)
    case GetRecDeclReply(len: Int)
    case GetFunSpecReply(len: Int)
    case GetOverloadedFunSpecReply(len: Int)
    case GetCallbacksReply(len: Int)
    case CannotCompleteRequest
  }

  private val replyCodec: JsonValueCodec[Reply] = JsonCodecMaker.make(
    CodecMakerConfig
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  private val requestCodec: JsonValueCodec[Request] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )
}
