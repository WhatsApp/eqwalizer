/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import scala.collection.mutable.ListBuffer

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.Forms._

// Expander uses Expand to expand the forms,
// but also it accumulates all the expansion errors into `InvalidForm`s
private object Expander {
  def expandStub(stub: ExtModuleStub): ModuleStub = {
    val module: String = stub.module
    val expand = new Expand(module)
    val invalidForms: ListBuffer[InvalidForm] = ListBuffer.empty
    val specs: ListBuffer[FunSpec] = ListBuffer.empty
    val overloadedSpecs: ListBuffer[OverloadedFunSpec] = ListBuffer.empty
    val records: ListBuffer[RecDecl] = ListBuffer.empty
    val callbacks: ListBuffer[Callback] = ListBuffer.empty
    val types: ListBuffer[TypeDecl] = ListBuffer.empty
    val privateOpaques: ListBuffer[TypeDecl] = ListBuffer.empty
    val publicOpaques: ListBuffer[OpaqueTypeDecl] = ListBuffer.empty

    val moduleFile = stub.forms.collectFirst { case f: File => f }.get.file
    var exports: Set[Id] = Set.empty
    var imports: Map[Id, String] = Map.empty
    var exportTypes: Set[Id] = Set.empty
    val behaviours: ListBuffer[Behaviour] = ListBuffer.empty
    var optionalCallbacks: Set[Id] = Set.empty
    var currentFile = moduleFile
    val convertTypes = new ConvertTypes(module)

    def typeDecl(t: ExternalTypeDecl): Unit =
      expand.expandTypeDecl(t) match {
        case Right(decl) =>
          convertTypes.convertTypeDecl(decl) match {
            case tDecl: TypeDecl =>
              types.addOne(tDecl)
            case invalid: InvalidForm =>
              if (currentFile == moduleFile)
                invalidForms.addOne(invalid)
            case _ =>
              throw new IllegalStateException()
          }
        case Left(invalid) =>
          if (currentFile == moduleFile)
            invalidForms.addOne(invalid)
      }

    def opaqueDecl(t: ExternalOpaqueDecl): Unit =
      expand.expandOpaqueDecl(t) match {
        case Right(decl) =>
          publicOpaques.addOne(convertTypes.convertOpaqueDeclPublic(decl))
          convertTypes.convertOpaquePrivate(decl) match {
            case tDecl: TypeDecl =>
              privateOpaques.addOne(tDecl)
            case invalid: InvalidForm =>
              if (currentFile == moduleFile)
                invalidForms.addOne(invalid)
            case _ =>
              throw new IllegalStateException()
          }
        case Left(invalid) =>
          if (currentFile == moduleFile)
            invalidForms.addOne(invalid)
      }

    def recordDecl(r: ExternalRecDecl): Unit =
      expand.expandRecDecl(r) match {
        case Right(decl) =>
          convertTypes.convertRecDecl(decl) match {
            case recDecl: RecDecl =>
              records.addOne(recDecl)
            case invalid: InvalidForm =>
              if (currentFile == moduleFile)
                invalidForms.addOne(invalid)
            case _ =>
              throw new IllegalStateException()
          }
        case Left(invalid) =>
          if (currentFile == moduleFile)
            invalidForms.addOne(invalid)
      }

    def spec(s: ExternalFunSpec): Unit =
      expand.expandFunSpec(s) match {
        case Right(s1) =>
          if (s1.types.size == 1)
            specs.addOne(convertTypes.convertSpec(s1))
          else
            overloadedSpecs.addOne(convertTypes.convertOverloadedSpec(s1))
        case Left(invalid) =>
          if (currentFile == moduleFile)
            invalidForms.addOne(invalid)
      }

    def callback(cb: ExternalCallback): Unit =
      expand.expandCallback(cb) match {
        case Right(cb1) =>
          callbacks.addOne(convertTypes.convertCallback(cb1))
        case Left(invalid) =>
          if (currentFile == moduleFile)
            invalidForms.addOne(invalid)
      }

    for (f <- stub.forms)
      f match {
        case File(file, _)                  => currentFile = file
        case e: Export                      => exports ++= e.funs
        case i: Import                      => imports ++= i.funs.map(_ -> i.module)
        case e: ExportType                  => exportTypes ++= e.types
        case t: ExternalTypeDecl            => typeDecl(t)
        case o: ExternalOpaqueDecl          => opaqueDecl(o)
        case s: ExternalFunSpec             => spec(s)
        case r: ExternalRecDecl             => recordDecl(r)
        case b: Behaviour                   => behaviours.addOne(b)
        case cb: ExternalCallback           => callback(cb)
        case ocb: ExternalOptionalCallbacks => optionalCallbacks ++= ocb.ids
        case _                              =>
      }

    types.addAll(ExtraTypes.typeDecls.getOrElse(module, Nil))

    ModuleStub(
      module,
      exports,
      imports,
      exportTypes,
      privateOpaques.map(decl => decl.id -> decl).toMap,
      publicOpaques.map(decl => decl.id -> decl).toMap,
      types.map(decl => decl.id -> decl).toMap,
      specs.map(s => s.id -> s).toMap,
      overloadedSpecs.map(is => is.id -> is).toMap,
      records.map(r => r.name -> r).toMap,
      callbacks.toList,
      optionalCallbacks,
      invalidForms.toList,
    )
  }
}
