/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import scala.collection.Searching
import scala.collection.mutable.ArrayBuilder

/** Do not use these utils for ELP integration: ELP owns location reporting.
  */
object Lines {
  def asLine(pos: Pos, lineBreaks: Array[Int]): Int = pos match {
    case span: TextRange =>
      assert(lineBreaks.head == 0)
      lineBreaks.search(span.startByte) match {
        case Searching.Found(index) =>
          index + 1
        case Searching.InsertionPoint(point) =>
          point
      }
    case LineAndColumn(line, _column) =>
      line
  }

  def toLineBreaks(bytes: Array[Byte]): Array[Int] = {
    val breakByte = '\n'.toByte
    val res = ArrayBuilder.make[Int]
    res += 0
    var i = 0
    while (i < bytes.length) {
      if (bytes(i) == breakByte) {
        res += i
      }
      i += 1
    }
    res.result()
  }
}
