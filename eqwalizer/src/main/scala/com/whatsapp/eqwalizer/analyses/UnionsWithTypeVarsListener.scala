/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.ExternalTypes.{ConstrainedFunType, FunExtType}
import com.whatsapp.eqwalizer.ast.{AstListener, ExternalTypeVars, Forms, Id, RemoteId}

import scala.collection.mutable.ListBuffer

class UnionsWithTypeVarsListener extends AstListener {

  private val unionsWithTypeVarsBuffer = ListBuffer[RemoteId]()
  private var module: Option[String] = None

  def unionsWithTypeVars: List[RemoteId] = unionsWithTypeVarsBuffer.toList

  override def enterModule(m: String, erlFile: String): Unit =
    module = Some(m)

  override def exitModule(_m: String): Unit =
    module = None

  override def enterFunSpec(spec: Forms.ExternalFunSpec): Unit =
    spec.types.map { case ConstrainedFunType(FunExtType(argTys, _), _) =>
      argTys.flatMap(ExternalTypeVars.findTypeVarInUnion).headOption match {
        case Some(vt) =>
          val Id(name, arity) = spec.id
          val rid = RemoteId(module.get, name, arity)
          unionsWithTypeVarsBuffer += rid
        case None => spec
      }
    }
}
