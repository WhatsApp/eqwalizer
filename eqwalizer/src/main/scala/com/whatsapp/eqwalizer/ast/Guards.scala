/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

object Guards {
  case class Guard(tests: List[Test])

  sealed trait Test { val pos: Pos }
  case class TestVar(v: String)(val pos: Pos) extends Test
  case class TestAtom(s: String)(val pos: Pos) extends Test
  case class TestNumber(lit: Option[Int])(val pos: Pos) extends Test
  case class TestTuple(elems: List[Test])(val pos: Pos) extends Test
  case class TestString()(val pos: Pos) extends Test
  case class TestNil()(val pos: Pos) extends Test
  case class TestCons(h: Test, t: Test)(val pos: Pos) extends Test
  case class TestCall(id: Id, args: List[Test])(val pos: Pos) extends Test
  case class TestRecordCreate(recName: String, fields: List[TestRecordField])(val pos: Pos) extends Test
  case class TestRecordSelect(rec: Test, recName: String, fieldName: String)(val pos: Pos) extends Test
  case class TestRecordIndex(recName: String, fieldName: String)(val pos: Pos) extends Test
  case class TestMapCreate(kvs: List[(Test, Test)])(val pos: Pos) extends Test
  case class TestMapUpdate(map: Test, kvs: List[(Test, Test)])(val pos: Pos) extends Test

  case class TestUnOp(op: String, arg: Test)(val pos: Pos) extends Test
  case class TestBinOp(op: String, arg1: Test, arg2: Test)(val pos: Pos) extends Test
  // Simplification. See https://fburl.com/binaryguards
  case class TestBinaryLit()(val pos: Pos) extends Test

  sealed trait TestRecordField { val value: Test }
  case class TestRecordFieldNamed(name: String, value: Test) extends TestRecordField
  case class TestRecordFieldGen(value: Test) extends TestRecordField
}
