/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Forms._

package object ast {
  case class Id(name: String, arity: Int) {
    override def toString: String = s"$name/$arity"
  }
  case class RemoteId(module: String, name: String, arity: Int) {
    override def toString: String = s"$module:$name/$arity"
  }

  sealed trait Pos extends Product
  case class TextRange(startByte: Int, endByte: Int) extends Pos
  object TextRange { val fake = TextRange(0, 100) }
  case class LineAndColumn(line: Int, column: Int) extends Pos
  object LineAndColumn { val fake = LineAndColumn(1, 100) }

  case class App(
      name: String,
      /** Dir for ebins corresponding to modules in rebar3 "src_dirs".
       * Does not include files in rebar3 "extra_src_dirs", such as tests
       */
      ebinDir: String,
      modules: List[String],
      // whether an app has a `.eqwalizer` file in its root
      hasEqwalizerMarker: Boolean,
  )

  case class ExtModuleStub(
      module: String,
      forms: List[ExternalForm],
  )
}
