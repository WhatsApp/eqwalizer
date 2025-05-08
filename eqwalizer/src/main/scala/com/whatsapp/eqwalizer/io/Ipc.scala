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

  sealed trait ASTFormat
  case object ConvertedForms extends ASTFormat
  case object TransitiveStub extends ASTFormat

  def getAstBytes(module: String, kind: ASTFormat): Option[Array[Byte]] = {
    send(GetAstBytes(module, kind))
    receive() match {
      case Right(GetAstBytesReply(len)) if len == 0 =>
        println()
        Console.out.flush()
        None
      case Right(GetAstBytesReply(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for $module but got $read")
        Some(buf)
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getAstBytes] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        // Happens when the client panics, such as ELP bug T111364923.
        // This error will only show up in logging, not really user-facing
        Console.err.println(s"eqWAlizer [getAstBytes] GotNull")
        throw Terminated
    }
  }

  def getTypeDecl(module: String, id: Id): Option[Array[Byte]] = {
    send(GetTypeDecl(module, id))
    receive() match
      case Right(GetTypeDeclReply(len)) =>
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
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getTypeDecl] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getTypeDecl] GotNull")
        throw Terminated
  }

  def getRecDecl(module: String, id: String): Option[Array[Byte]] = {
    send(GetRecDecl(module, id))
    receive() match
      case Right(GetRecDeclReply(len)) =>
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
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getRecDecl] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getRecDecl] GotNull")
        throw Terminated
  }

  def getFunSpec(module: String, id: Id): Option[Array[Byte]] = {
    send(GetFunSpec(module, id))
    receive() match
      case Right(GetFunSpecReply(len)) =>
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
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getFunSpec] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getFunSpec] GotNull")
        throw Terminated
  }

  def getOverloadedFunSpec(module: String, id: Id): Option[Array[Byte]] = {
    send(GetOverloadedFunSpec(module, id))
    receive() match
      case Right(GetOverloadedFunSpecReply(len)) =>
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
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getOverloadedFunSpec] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getOverloadedFunSpec] GotNull")
        throw Terminated
  }

  def getCallbacks(module: String): Option[Array[Byte]] = {
    send(GetCallbacks(module))
    receive() match
      case Right(GetCallbacksReply(len)) =>
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
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [getCallbacks] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [getCallbacks] GotNull")
        throw Terminated
  }

  def sendDone(diagnostics: Map[String, List[ELPDiagnostics.Error]], typeInfo: Map[String, List[(Pos, Type)]]): Unit =
    send(Done(diagnostics, typeInfo))

  def sendEqwalizingStart(module: String): Unit =
    send(EqwalizingStart(module))

  def sendEqwalizingDone(module: String): Unit =
    send(EqwalizingDone(module))

  def validateType(ty: ExtType): (Boolean, Array[Byte]) = {
    send(ValidateType(ty))
    receive() match {
      case Right(ValidatedType(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for validated type but got $read")
        (true, buf)
      case Right(InvalidType(len)) =>
        println()
        Console.out.flush()
        val buf = new Array[Byte](len)
        val read = readNBytes(System.in, buf, 0, len)
        assert(read == len, s"expected $len for validated type but got $read")
        (false, buf)
      case Right(CannotCompleteRequest) =>
        // The client has asked eqWAlizer to die
        throw Terminated
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [validateType] received bad reply from ELP when validating type: $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [validateType] GotNull")
        throw Terminated
    }
  }

  private def send(req: Request): Unit = {
    writeToStream(req, Console.out)(requestCodec)
    Console.out.println()
    Console.flush()
  }

  def shouldEqwalize(module: String): Boolean = {
    send(EnteringModule(module))
    receive() match {
      case Right(ELPEnteringModule) =>
        true
      case Right(ELPExitingModule) =>
        false
      case Right(reply) =>
        Console.err.println(s"eqWAlizer [shouldEqwalize] received bad reply from ELP $reply")
        throw Terminated
      case Left(GotNull()) =>
        Console.err.println(s"eqWAlizer [shouldEqwalize] GotNull")
        throw Terminated
    }
  }

  def finishEqwalization(
      diagnostics: Map[String, List[ELPDiagnostics.Error]],
      deps: List[String],
      typeInfo: Map[String, List[(Pos, Type)]],
  ): Unit = {
    send(Dependencies(deps))
    sendDone(diagnostics, typeInfo)
    receive() match {
      case Right(ELPExitingModule) =>
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
    var n = 0;

    while (n < len) {
      val count = stream.read(b, off + n, len - n);
      if (count < 0)
        return n
      n += count;
    }
    n
  }

  private sealed trait Request
  private case class EnteringModule(module: String) extends Request
  private case class ExitingModule(module: String) extends Request
  private case class GetAstBytes(module: String, format: ASTFormat) extends Request
  private case class EqwalizingStart(module: String) extends Request
  private case class EqwalizingDone(module: String) extends Request
  private case class Dependencies(modules: List[String]) extends Request
  private case class Done(
      diagnostics: Map[String, List[ELPDiagnostics.Error]],
      typeInfo: Map[String, List[(Pos, Type)]],
  ) extends Request
  private case class ValidateType(ty: ExtType) extends Request
  private case class GetTypeDecl(module: String, id: Id) extends Request
  private case class GetRecDecl(module: String, id: String) extends Request
  private case class GetFunSpec(module: String, id: Id) extends Request
  private case class GetOverloadedFunSpec(module: String, id: Id) extends Request
  private case class GetCallbacks(module: String) extends Request

  private sealed trait Reply
  private case object ELPEnteringModule extends Reply
  private case object ELPExitingModule extends Reply

  /**
    * Classes with `len` are non-JSON part of the protocol.
    * After receiving these messages, eqWAlizer prints a newline to stdout
    * and then reads `len` or `typeBytesLen` bytes from stdin
    */
  private case class GetAstBytesReply(len: Int) extends Reply
  private case class ValidatedType(len: Int) extends Reply
  private case class InvalidType(len: Int) extends Reply
  private case class GetTypeDeclReply(len: Int) extends Reply
  private case class GetRecDeclReply(len: Int) extends Reply
  private case class GetFunSpecReply(len: Int) extends Reply
  private case class GetOverloadedFunSpecReply(len: Int) extends Reply
  private case class GetCallbacksReply(len: Int) extends Reply
  private case object CannotCompleteRequest extends Reply

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
