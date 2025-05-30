/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import com.whatsapp.eqwalizer.ast.Forms.{Callback, FunSpec, OverloadedFunSpec, RecDecl, TypeDecl}
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.io.Ipc

import scala.collection.mutable

// This object proxies to existing methods inside ELP with similar signatures.
// Its purpose is to make porting to Rust more isolated and structured.
object ELPProxy {
  // the set of "loaded" modules to track dependencies
  private val modules: mutable.Set[String] = mutable.Set.empty
  // the set of used modules in the current session
  def depModules(): Set[String] = {
    val result = modules.toSet ++ Set("eqwalizer_types")
    modules.clear()
    result
  }
  // Caches (similar to salsa caches)

  private val typeDeclCache: mutable.Map[(String, Id), Option[TypeDecl]] = mutable.Map.empty
  private val recDeclCache: mutable.Map[(String, String), Option[RecDecl]] = mutable.Map.empty
  private val funSpecCache: mutable.Map[(String, Id), Option[FunSpec]] = mutable.Map.empty
  private val overloadedFunSpecCache: mutable.Map[(String, Id), Option[OverloadedFunSpec]] = mutable.Map.empty
  private val callbacksCache: mutable.Map[String, Option[(List[Callback], Set[Id])]] = mutable.Map.empty

  // jsoniter_scala codecs boilerplate

  private val typeDeclCodec: JsonValueCodec[TypeDecl] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  private val recDeclCodec: JsonValueCodec[RecDecl] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  private val funSpecCodec: JsonValueCodec[FunSpec] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  private val overloadedFunSpecCodec: JsonValueCodec[OverloadedFunSpec] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  private val callbacksCodec: JsonValueCodec[(List[Callback], Set[Id])] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )

  // each method below has a counterpart in ELP

  // EqwalizerDiagnosticsDatabase::type_decl
  def typeDecl(module: String, id: Id): Option[TypeDecl] = {
    modules.addOne(module)
    val key = (module, id)
    typeDeclCache.get(key) match
      case Some(value) =>
        value
      case None =>
        val optTypeDecl = Ipc.getTypeDecl(module, id).map(readFromArray[TypeDecl](_)(typeDeclCodec))
        typeDeclCache.put(key, optTypeDecl)
        optTypeDecl
  }

  // EqwalizerDiagnosticsDatabase::rec_decl
  def recDecl(module: String, id: String): Option[RecDecl] = {
    modules.addOne(module)
    val key = (module, id)
    recDeclCache.get(key) match
      case Some(value) =>
        value
      case None =>
        val optRecDecl = Ipc.getRecDecl(module, id).map(readFromArray[RecDecl](_)(recDeclCodec))
        recDeclCache.put(key, optRecDecl)
        optRecDecl
  }

  // EqwalizerDiagnosticsDatabase::fun_spec
  def funSpec(module: String, id: Id): Option[FunSpec] = {
    modules.addOne(module)
    val key = (module, id)
    funSpecCache.get(key) match
      case Some(value) =>
        value
      case None =>
        val optFunSpec = Ipc.getFunSpec(module, id).map(readFromArray[FunSpec](_)(funSpecCodec))
        funSpecCache.put(key, optFunSpec)
        optFunSpec
  }

  // EqwalizerDiagnosticsDatabase::overloaded_fun_spec
  def overloadedFunSpec(module: String, id: Id): Option[OverloadedFunSpec] = {
    modules.addOne(module)
    val key = (module, id)
    overloadedFunSpecCache.get(key) match
      case Some(value) =>
        value
      case None =>
        val optOverloadedFunSpec =
          Ipc.getOverloadedFunSpec(module, id).map(readFromArray[OverloadedFunSpec](_)(overloadedFunSpecCodec))
        overloadedFunSpecCache.put(key, optOverloadedFunSpec)
        optOverloadedFunSpec
  }

  def callbacks(module: String): Option[(List[Callback], Set[Id])] = {
    modules.addOne(module)
    val key = module
    callbacksCache.get(key) match
      case Some(value) =>
        value
      case None =>
        val optCallbacks = Ipc.getCallbacks(module).map(readFromArray[(List[Callback], Set[Id])](_)(callbacksCodec))
        callbacksCache.put(key, optCallbacks)
        optCallbacks
  }
}
