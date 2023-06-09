/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang._
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.ast.stub.DbApi.{AstBeam, AstEtfFile, AstEtfIpc}
import com.whatsapp.eqwalizer.io.EData.EObject

import java.io.DataInputStream
import java.nio.file.Files

/** Loads abstract forms in beam or ELP etf formats
  */
object AstLoader {
  val Abst = 1096971124
  val Dbgi = 1147299689

  def loadAbstractForms(astData: DbApi.AstBeamEtfStorage): Option[EObject] =
    loadAbstractFormsJ(astData).map(EData.fromJava)

  def loadAbstractFormsJ(astStorage: DbApi.AstBeamEtfStorage, stubOnly: Boolean = false): Option[OtpErlangList] =
    astStorage match {
      case AstBeam(path) =>
        val bytes = Files.readAllBytes(path)
        Some(loadBeamJ(bytes))
      case AstEtfFile(path) =>
        val bytes = Files.readAllBytes(path)
        Some(loadEtfJ(bytes, path.toString))
      case AstEtfIpc(module) =>
        val format =
          if (stubOnly) Ipc.RawStub
          else Ipc.RawForms
        Ipc
          .getAstBytes(module, format)
          .map(bytes => loadEtfJ(bytes, originForDebugging = s"from IPC request for $module"))
    }

  private def loadBeamJ(bytes: Array[Byte]): OtpErlangList = {
    val byteInputStream = new OtpInputStream(bytes)
    val input = new DataInputStream(byteInputStream)
    // "FOR1"
    input.readInt
    // length
    input.readInt
    // BEAM
    input.readInt
    var result: OtpErlangList = null
    while (result == null && input.available() > 0) {
      val intTag = input.readInt
      val length = input.readInt
      if (intTag == Dbgi || intTag == Abst) {
        val t1 = byteInputStream.read_any.asInstanceOf[OtpErlangTuple]
        val t2 = t1.elementAt(2).asInstanceOf[OtpErlangTuple]
        result = t2.elementAt(0).asInstanceOf[OtpErlangList]
      } else byteInputStream.skip((length + 3) & ~3)
    }
    result
  }

  private def loadEtfJ(bytes: Array[Byte], originForDebugging: String): OtpErlangList = {
    val byteInputStream = new OtpInputStream(bytes)
    // "etf": ({ok, Forms}) | (tuple() with errors/warnings)
    val resultTuple = byteInputStream.read_any().asInstanceOf[OtpErlangTuple]
    val statusMarker = resultTuple.elementAt(0).asInstanceOf[OtpErlangAtom]
    assert(statusMarker.atomValue() == "ok", s"ok status is expected from elp ast. origin: $originForDebugging")
    val forms = resultTuple.elementAt(1).asInstanceOf[OtpErlangList]
    forms
  }
}
