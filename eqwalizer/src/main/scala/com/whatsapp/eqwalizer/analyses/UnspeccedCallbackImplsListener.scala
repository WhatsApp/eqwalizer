/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import com.whatsapp.eqwalizer.ast.Forms.{Behaviour, ExternalCallback, ExternalFunSpec}
import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.stub.DbApi

import java.nio.file.{Files, Paths}

class UnspeccedCallbackImplsListener(val module: String) extends AstListener {
  private val generatedMark: String = "@" + "generated"
  private var generatedMod: Boolean = false
  private val stub = DbApi.getExtModuleStub(module).get
  private val specIds = stub.forms.collect({ case ExternalFunSpec(id, _) => id }).toSet
  private val callbackInterfaces: Set[Id] = stub.forms
    .collect({ case Behaviour(m) => m })
    .flatMap { m =>
      DbApi.getExtModuleStub(m) match {
        case Some(stub) =>
          stub.forms.collect({ case ExternalCallback(id, _) => id }).toSet
        case None =>
          Nil
      }
    }
    .toSet
  private var unspeccedCallbackImpls: Set[Id] = Set.empty

  override def enterModule(m: String, erlFile: String): Unit = {
    val contents = new String(Files.readAllBytes(Paths.get(erlFile)))
    val preamble = contents.take(200)
    generatedMod = preamble.contains(generatedMark)
  }

  override def enterFunDecl(funDecl: Forms.FunDecl): Unit = {
    val id = funDecl.id
    if (!specIds.contains(id) && callbackInterfaces(id))
      unspeccedCallbackImpls += id
  }

  def getUnspeccedCallbackImpls: Set[Id] =
    unspeccedCallbackImpls
  def isGenerated: Boolean =
    generatedMod
}
