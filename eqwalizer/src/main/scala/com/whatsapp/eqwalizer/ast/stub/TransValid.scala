/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import scala.collection.mutable.ListBuffer

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._

private class TransValid {
  import TransValid._

  private var inProgress = Set[Ref]()
  private var invalidRefs = Map[Ref, List[Ref]]()

  def checkStub(stub: ModuleStub): ModuleStub = {
    val module = stub.module
    val typesMut = ListBuffer[TypeDecl]()
    val privateOpaquesMut = ListBuffer[TypeDecl]()
    val publicOpaquesMut = ListBuffer[OpaqueTypeDecl]()
    val specsMut = ListBuffer[FunSpec]()
    val overloadedSpecsMut = ListBuffer[OverloadedFunSpec]()
    val recsMut = ListBuffer[RecDecl]()
    val invalidFormsMut = ListBuffer.from(stub.invalidForms)

    stub.types.values.foreach(addTypeDecl(module, _, typesMut, invalidFormsMut))
    stub.privateOpaques.values.foreach(addTypeDecl(module, _, privateOpaquesMut, invalidFormsMut))
    stub.publicOpaques.values.foreach(addOpaqueDecl(module, _, publicOpaquesMut))
    stub.records.values.foreach(addRecord(module, _, recsMut, invalidFormsMut))
    stub.specs.values.foreach(addSpec(module, _, specsMut, invalidFormsMut))
    stub.overloadedSpecs.values.foreach(addOverloadedSpec(module, _, overloadedSpecsMut, invalidFormsMut))
    val callbacks = stub.callbacks.map(removeInvalidTypesFromCallback(module, _))

    ModuleStub(
      module = module,
      exports = stub.exports,
      imports = stub.imports,
      exportTypes = stub.exportTypes,
      types = typesMut.map(decl => decl.id -> decl).toMap,
      privateOpaques = privateOpaquesMut.map(decl => decl.id -> decl).toMap,
      publicOpaques = publicOpaquesMut.map(decl => decl.id -> decl).toMap,
      specs = specsMut.map(spec => spec.id -> spec).toMap,
      overloadedSpecs = overloadedSpecsMut.map(spec => spec.id -> spec).toMap,
      records = recsMut.map(rec => rec.name -> rec).toMap,
      callbacks = callbacks,
      optionalCallbacks = stub.optionalCallbacks,
      invalidForms = invalidFormsMut.toList,
    )
  }

  private def addTypeDecl(
      module: String,
      decl: TypeDecl,
      declsMut: ListBuffer[TypeDecl],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit = {
    val ref = RidRef(RemoteId(module, decl.id.name, decl.id.arity))
    if (isValid(ref)) declsMut.addOne(decl)
    else {
      val invalids = invalidRefs(ref).map(show(module, _))
      val diag = InvalidDiagnostics.TransitiveInvalid(decl.pos, decl.id.toString, invalids)
      invalidsMut.addOne(InvalidTypeDecl(decl.id, diag)(decl.pos))
    }
  }

  private def addOpaqueDecl(
      module: String,
      decl: OpaqueTypeDecl,
      declsMut: ListBuffer[OpaqueTypeDecl],
  ): Unit = {
    val ref = RidRef(RemoteId(module, decl.id.name, decl.id.arity))
    if (isValid(ref)) declsMut.addOne(decl)
  }

  private def addSpec(
      module: String,
      spec: FunSpec,
      specsMut: ListBuffer[FunSpec],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit =
    collectInvalidReferences(module, spec.ty) match {
      case Nil =>
        specsMut.addOne(spec)
      case invalids =>
        val diag =
          InvalidDiagnostics.TransitiveInvalid(spec.pos, spec.id.toString, invalids.distinct.map(show(module, _)))
        invalidsMut.addOne(InvalidFunSpec(spec.id, diag)(spec.pos))
    }

  private def addRecord(
      module: String,
      rec: RecDecl,
      recsMut: ListBuffer[RecDecl],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit = {
    val ref = RecRef(module, rec.name)
    if (isValid(ref)) recsMut.addOne(rec)
    else {
      val invalids = invalidRefs(ref).map(show(module, _))
      val diag = InvalidDiagnostics.TransitiveInvalid(rec.pos, rec.name, invalids)
      invalidsMut.addOne(InvalidRecDecl(rec.name, diag)(rec.pos))
    }
  }

  private def addOverloadedSpec(
      module: String,
      spec: OverloadedFunSpec,
      overloadedSpecsMut: ListBuffer[OverloadedFunSpec],
      invalidsMut: ListBuffer[InvalidForm],
  ): Unit = {
    spec.tys.flatMap(collectInvalidReferences(module, _)) match {
      case Nil =>
        overloadedSpecsMut.addOne(spec)
      case invalids =>
        val diag =
          InvalidDiagnostics.TransitiveInvalid(spec.pos, spec.id.toString, invalids.distinct.map(show(module, _)))
        invalidsMut.addOne(InvalidFunSpec(spec.id, diag)(spec.pos))
    }
  }

  /** Filter out any invalid callback types.
    * A callback for which each of its types is invalid will not be
    * used for checking or synthesizing types of its implementations.
    */
  private def removeInvalidTypesFromCallback(
      module: String,
      cb: Callback,
  ): Callback =
    cb.copy(
      tys = cb.tys.filter(collectInvalidReferences(module, _).isEmpty)
    )(cb.pos)

  private def isValid(ref: Ref): Boolean = {
    if (inProgress(ref)) true
    else if (invalidRefs.contains(ref)) invalidRefs(ref).isEmpty
    else {
      inProgress += ref
      val invalids = Db.getValidatedModuleStub(ref.refModule) match {
        case Some(cStub) =>
          ref match {
            case RidRef(rid) =>
              val id = Id(rid.name, rid.arity)
              cStub.types.get(id) match {
                case Some(TypeDecl(_, _, body)) =>
                  collectInvalidReferences(rid.module, body)
                case None =>
                  cStub.privateOpaques.get(id) match {
                    case Some(TypeDecl(_, _, body)) =>
                      collectInvalidReferences(rid.module, body)
                    case None =>
                      List(ref)
                  }
              }
            case RecRef(refModule, name) =>
              cStub.records.get(name) match {
                case Some(RecDecl(_, fields, _)) =>
                  fields.flatMap(_.tp).flatMap(collectInvalidReferences(refModule, _))
                case None =>
                  List(ref)
              }
          }
        case None =>
          List(ref)
      }
      inProgress -= ref
      invalidRefs += ref -> invalids.distinct
      invalids.isEmpty
    }
  }

  private def collectInvalidReferences(module: String, ty: Type): List[Ref] = ty match {
    case RemoteType(rid, argTys) =>
      val invalids = argTys.flatMap(collectInvalidReferences(module, _))
      val ref = RidRef(rid)
      if (isValid(ref)) invalids
      else ref :: invalids
    case OpaqueType(_rid, _argTys) =>
      // $COVERAGE-OFF$
      throw new IllegalStateException("unreachable because we lazily convert RemoteType to OpaqueType")
    // $COVERAGE-ON$
    case RecordType(name) =>
      val ref = RecRef(module, name)
      if (isValid(ref)) Nil
      else List(ref)
    case t: RefinedRecordType =>
      val ref = RecRef(module, t.recType.name)
      val invalids = TypeVars.children(t).flatMap(collectInvalidReferences(module, _))
      if (isValid(ref)) invalids
      else ref :: invalids
    case _ =>
      TypeVars.children(ty).flatMap(collectInvalidReferences(module, _))
  }

  private def show(module: String, ref: Ref): String = ref match {
    case RidRef(rid) if rid.module == module =>
      Id(rid.name, rid.arity).toString
    case RidRef(rid) =>
      rid.toString
    case RecRef(refModule, name) =>
      assert(refModule == module)
      s"#$name{}"
  }
}

private object TransValid {
  private sealed trait Ref {
    def refModule: String
  }
  private case class RidRef(rid: RemoteId) extends Ref {
    override def refModule: String = rid.module
  }
  private case class RecRef(refModule: String, name: String) extends Ref
}
