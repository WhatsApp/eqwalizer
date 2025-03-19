/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id

private case class ModuleStub(
    module: String,
    exports: Set[Id],
    imports: Map[Id, String],
    exportTypes: Set[Id],
    opaques: Map[Id, TypeDecl],
    types: Map[Id, TypeDecl],
    specs: Map[Id, FunSpec],
    overloadedSpecs: Map[Id, OverloadedFunSpec],
    records: Map[String, RecDecl],
    callbacks: List[Callback],
    optionalCallbacks: Set[Id],
    invalidForms: List[InvalidForm],
)

private object ModuleStub {
  implicit val codec: JsonValueCodec[ModuleStub] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper {
        case "pos" => "location"
        case "mod" => "module"
        case s     => JsonCodecMaker.enforce_snake_case(s)
      }
  )
}
