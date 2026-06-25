/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

enum Specifier {
  case SignedInteger, UnsignedInteger, Float, Binary, Bytes, Bitstring, Bits, Utf8, Utf16, Utf32
}

object Specifier {
  def expType(s: Specifier, stringLiteral: Boolean): Type =
    s match {
      case UnsignedInteger | Utf8 | Utf16 | Utf32 =>
        if (stringLiteral) stringType
        else IntegerType
      case SignedInteger =>
        IntegerType
      case Float =>
        FloatType
      case Binary | Bytes | Bitstring | Bits =>
        BinaryType
    }
}
