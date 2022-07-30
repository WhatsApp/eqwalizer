/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.BinarySpecifiers.Specifier
import com.whatsapp.eqwalizer.ast.Exprs.Expr

object Pats {
  sealed trait Pat { val pos: Pos }

  case class PatWild()(val pos: Pos) extends Pat
  case class PatMatch(pat: Pat, arg: Pat)(val pos: Pos) extends Pat
  case class PatTuple(elems: List[Pat])(val pos: Pos) extends Pat

  case class PatString()(val pos: Pos) extends Pat

  case class PatNil()(val pos: Pos) extends Pat
  case class PatCons(h: Pat, t: Pat)(val pos: Pos) extends Pat

  case class PatInt()(val pos: Pos) extends Pat
  case class PatNumber()(val pos: Pos) extends Pat
  case class PatAtom(s: String)(val pos: Pos) extends Pat
  case class PatVar(n: String)(val pos: Pos) extends Pat
  case class PatRecord(recName: String, fields: List[PatRecordFieldNamed], gen: Option[Pat])(val pos: Pos) extends Pat
  case class PatRecordIndex(recName: String, fieldName: String)(val pos: Pos) extends Pat

  case class PatUnOp(op: String, arg: Pat)(val pos: Pos) extends Pat
  case class PatBinOp(op: String, arg1: Pat, arg2: Pat)(val pos: Pos) extends Pat

  case class PatBinary(elems: List[PatBinaryElem])(val pos: Pos) extends Pat
  case class PatBinaryElem(pat: Pat, size: Option[Expr], specifier: Specifier)(val pos: Pos)

  case class PatRecordFieldNamed(name: String, pat: Pat)
  case class PatMap(kvs: List[(Pat, Pat)])(val pos: Pos) extends Pat
}
