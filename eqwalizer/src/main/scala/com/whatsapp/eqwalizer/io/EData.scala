/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang._

object EData {
  sealed trait EObject
  case class EAtom(atom: String) extends EObject
  case class EBitStr(bin: Array[Byte], pad_bits: Int) extends EObject
  case class EDouble(d: Double) extends EObject
  case class EExternalFun(module: String, function: String, arity: Int) extends EObject
  case class EList(elems: List[EObject], lastTail: Option[EObject]) extends EObject
  case class ELong(value: BigInt) extends EObject
  case class EMap(entries: List[(EObject, EObject)]) extends EObject
  case class EString(str: String) extends EObject
  case class ETuple(elems: List[EObject]) extends EObject
  case object EPid extends EObject
  case object EPort extends EObject
  case object ERef extends EObject
  case object EFun extends EObject

  def fromJava(jObject: OtpErlangObject): EObject =
    jObject match {
      case otpAtom: OtpErlangAtom =>
        EAtom(otpAtom.atomValue().intern())
      case otpBitstr: OtpErlangBitstr =>
        EBitStr(otpBitstr.binaryValue(), otpBitstr.pad_bits())
      case otpDouble: OtpErlangDouble =>
        EDouble(otpDouble.doubleValue())
      case otpList: OtpErlangList =>
        val elems = otpList.elements().toList.map(fromJava)
        val lastTail = Option(otpList.getLastTail).map(fromJava)
        EList(elems, lastTail)
      case otpLong: OtpErlangLong =>
        ELong(otpLong.bigIntegerValue())
      case otpMap: OtpErlangMap =>
        val otpKeys = otpMap.keys()
        val otpValues = otpKeys.map(k => otpMap.get(k))
        val eKeys = otpKeys.toList.map(fromJava)
        val eValues = otpValues.toList.map(fromJava)
        EMap(eKeys.zip(eValues))
      case otpString: OtpErlangString =>
        EString(otpString.stringValue())
      case otpTuple: OtpErlangTuple =>
        val elems = otpTuple.elements().toList.map(fromJava)
        ETuple(elems)
      case _: OtpErlangPid =>
        EPid
      case _: OtpErlangPort =>
        EPort
      case _: OtpErlangRef =>
        ERef
      case _: OtpErlangFun =>
        EFun
      case _: OtpErlangExternalFun =>
        EFun
    }

  def toJava(eObject: EObject): OtpErlangObject = eObject match {
    case EAtom(a) =>
      new OtpErlangAtom(a)
    case ELong(l) =>
      new OtpErlangLong(l.longValue)
    case ETuple(elems) =>
      new OtpErlangTuple(elems.map(toJava).toArray)
    case EList(elems, None) =>
      new OtpErlangList(elems.map(toJava).toArray)
    case _ =>
      throw new IllegalStateException()
  }

  trait Visitor {
    def visit(obj: EObject): Unit = ()
  }

  def traverse(obj: EObject, visitor: Visitor): Unit = obj match {
    case eAtom: EAtom =>
      visitor.visit(eAtom)
    case eBitStr: EBitStr =>
      visitor.visit(eBitStr)
    case ed: EDouble =>
      visitor.visit(ed)
    case efun: EExternalFun =>
      visitor.visit(efun)
    case elist @ EList(elems, _) =>
      visitor.visit(elist)
      elems.foreach(traverse(_, visitor))
    case el: ELong =>
      visitor.visit(el)
    case emap @ EMap(entries) =>
      visitor.visit(emap)
      entries.foreach { case (k, v) =>
        traverse(k, visitor)
        traverse(v, visitor)
      }
    case eString: EString =>
      visitor.visit(eString)
    case etuple @ ETuple(elems) =>
      visitor.visit(etuple)
      elems.foreach(traverse(_, visitor))
    case _ =>
      ()
  }
}
