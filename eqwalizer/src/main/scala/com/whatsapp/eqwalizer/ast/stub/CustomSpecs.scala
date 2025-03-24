/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms.{FunSpec, OverloadedFunSpec}
import com.whatsapp.eqwalizer.ast.Id

private object CustomSpecs {
  private lazy val customSpecs: Map[String, Map[Id, FunSpec]] =
    Db.getModuleStub("eqwalizer_specs") match {
      case Some(stub) =>
        stub.specs.toList
          .map { case (id, spec) =>
            val Array(module, funName) = id.name.split(":")
            val id1 = Id(funName, id.arity)
            val spec1 = spec.copy(id = id1)
            (module, id1) -> spec1
          }
          .groupBy { case ((module, _), _) => module }
          .view
          .mapValues(_.map { case ((_, id), spec) => id -> spec }.toMap)
          .toMap
      case None =>
        Map.empty
    }

  private lazy val customOverloadedSpecs: Map[String, Map[Id, OverloadedFunSpec]] =
    Db.getModuleStub("eqwalizer_specs") match {
      case Some(stub) =>
        stub.overloadedSpecs.toList
          .map { case (id, spec) =>
            val Array(module, funName) = id.name.split(":")
            val id1 = Id(funName, id.arity)
            val spec1 = spec.copy(id = id1)
            (module, id1) -> spec1
          }
          .groupBy { case ((module, _), _) => module }
          .view
          .mapValues(_.map { case ((_, id), spec) => id -> spec }.toMap)
          .toMap
      case None =>
        Map.empty
    }

  def getSpec(module: String, id: Id): Option[FunSpec] =
    for {
      specs <- customSpecs.get(module)
      spec <- specs.get(id)
    } yield spec

  def getOverloadedSpec(module: String, id: Id): Option[OverloadedFunSpec] =
    for {
      specs <- customOverloadedSpecs.get(module)
      spec <- specs.get(id)
    } yield spec
}
