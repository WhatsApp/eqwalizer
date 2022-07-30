/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id

private case class ModuleStub(
    module: String,
    exports: Set[Id],
    imports: Map[Id, String],
    exportTypes: Set[Id],
    privateOpaques: Map[Id, TypeDecl],
    publicOpaques: Map[Id, OpaqueTypeDecl],
    types: Map[Id, TypeDecl],
    specs: Map[Id, FunSpec],
    overloadedSpecs: Map[Id, OverloadedFunSpec],
    records: Map[String, RecDecl],
    callbacks: List[Callback],
    optionalCallbacks: Set[Id],
    invalidForms: List[InvalidForm],
)
