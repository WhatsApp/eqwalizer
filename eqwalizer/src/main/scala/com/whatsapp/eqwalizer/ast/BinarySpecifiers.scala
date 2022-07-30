/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

object BinarySpecifiers {
  sealed trait Specifier
  case object SignedIntegerSpecifier extends Specifier
  case object UnsignedIntegerSpecifier extends Specifier
  case object FloatSpecifier extends Specifier
  case object BinarySpecifier extends Specifier
  case object BytesSpecifier extends Specifier
  case object BitstringSpecifier extends Specifier
  case object BitsSpecifier extends Specifier
  case object Utf8Specifier extends Specifier
  case object Utf16Specifier extends Specifier
  case object Utf32Specifier extends Specifier

  def expType(s: Specifier, stringLiteral: Boolean): Type =
    s match {
      case UnsignedIntegerSpecifier | Utf8Specifier | Utf16Specifier | Utf32Specifier =>
        if (stringLiteral) stringType
        else NumberType
      case SignedIntegerSpecifier =>
        NumberType
      case FloatSpecifier =>
        floatType
      case BinarySpecifier | BytesSpecifier | BitstringSpecifier | BitsSpecifier =>
        BinaryType
    }
}
