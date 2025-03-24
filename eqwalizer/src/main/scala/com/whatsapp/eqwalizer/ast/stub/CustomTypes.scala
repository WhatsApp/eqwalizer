/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.Forms.TypeDecl
import com.whatsapp.eqwalizer.ast.Id

private object CustomTypes {
  private lazy val customTypes: Map[String, Map[Id, TypeDecl]] =
    Db.getModuleStub("eqwalizer_types") match {
      case Some(stub) =>
        stub.types.toList
          .map { case (id, ty) =>
            val Array(module, tyName) = id.name.split(":")
            val id1 = Id(tyName, id.arity)
            val ty1 = ty.copy(id = id1)
            (module, id1) -> ty1
          }
          .groupBy { case ((module, _), _) => module }
          .view
          .mapValues(_.map { case ((_, id), ty) => id -> ty }.toMap)
          .toMap
      case None =>
        Map.empty
    }

  def getType(module: String, id: Id): Option[TypeDecl] =
    for {
      types <- customTypes.get(module)
      ty <- types.get(id)
    } yield ty
}
