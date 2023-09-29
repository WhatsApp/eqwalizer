/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms.TypeDecl
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.DbApi

object ExtraTypes {
  private val fakePos =
    if (DbApi.fromBeam("erlang")) LineAndColumn.fake
    else TextRange.fake

  private val builtinDecls = builtinTypeAliasBodies.map { case (name, body) =>
    TypeDecl(Id(name, 0), Nil, body, None)(fakePos)
  }.toList

  val typeDecls: Map[String, List[TypeDecl]] = Map(
    "erlang" -> builtinDecls
  )
}
