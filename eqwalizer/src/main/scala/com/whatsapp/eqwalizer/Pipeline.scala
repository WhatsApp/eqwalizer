/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.Types.{DynamicType, FunType}
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc.TcDiagnostics._
import com.whatsapp.eqwalizer.tc.{Options, PipelineContext, noOptions}

import scala.collection.mutable.ListBuffer

object Pipeline {
  def checkForms(
      moduleName: String,
      options: Options = noOptions,
  ): (List[TypeError], List[Invalid], List[RedundantFixme]) = {
    val invalids = Db.getInvalids(moduleName).get
    val forms = Forms.load(moduleName)
    val module = forms.collectFirst { case Module(m) => m }.get
    val erlFile = forms.collectFirst { case File(f, _) => f }.get
    val meta = forms.collectFirst { case meta: ElpMetadata => meta }
    var noCheckFuns = forms.collect { case f: EqwalizerNowarnFunction => (f.id, f.pos) }.toMap
    val unlimitedRefinementFuns = forms.collect { case EqwalizerUnlimitedRefinement(id) => id }.toSet
    var currentFile = erlFile
    val result = ListBuffer.empty[TypeError]
    for (form <- forms)
      form match {
        case f @ File(path, _) =>
          currentFile = path
        case f: FunDecl if currentFile == erlFile =>
          val options1 =
            if (unlimitedRefinementFuns(f.id)) options.copy(unlimitedRefinement = Some(true)) else options
          val ctx = PipelineContext(module, options1)
          val fErrors = (Db.getSpec(module, f.id), Db.getOverloadedSpec(module, f.id)) match {
            case (Some(spec), _) =>
              checkFun(ctx, f, spec)
            case (_, Some(overloadedSpec)) =>
              checkOverloadedFun(ctx, f, overloadedSpec)
            case _ =>
              checkFun(ctx, f, getDynamicFunSpecType(f))
          }
          if (noCheckFuns.contains(f.id)) {
            if (fErrors.isEmpty)
              result.addOne(RedundantNowarnFunction(noCheckFuns(f.id)))
            noCheckFuns = noCheckFuns.removed(f.id)
          } else result.addAll(fErrors)
        case b: Behaviour =>
          val ctx = PipelineContext(module, options)
          if (Db.isKnownModule(b.name)) {
            val (callbacks, optional) = Db.getCallbacks(b.name)
            result.addAll(
              callbacks.flatMap { cb =>
                ctx.checkCallback.checkImpl(module, b, cb, optional(cb.id))
              }
            )
          } else {
            result.addOne(NonexistentBehaviour(b.pos, b.name))
          }
        case _ =>
        // skipping things from header files
      }
    noCheckFuns.foreach { case (_, pos) => result.addOne(RedundantNowarnFunction(pos)) }
    val errors = result.toList
    applyFixmes(errors, invalids, meta)
  }

  private def getDynamicFunSpecType(f: FunDecl): FunSpec =
    FunSpec(f.id, FunType(Nil, List.fill(f.id.arity)(DynamicType), DynamicType))

  private def checkFun(ctx: PipelineContext, f: FunDecl, spec: FunSpec): List[TypeError] = {
    ctx.check.checkFun(f, spec)
    ctx.diagnosticsInfo.popErrors()
  }

  private def checkOverloadedFun(
      ctx: PipelineContext,
      f: FunDecl,
      overloadedSpec: OverloadedFunSpec,
  ): List[TypeError] = {
    ctx.check.checkOverloadedFun(f, overloadedSpec)
    ctx.diagnosticsInfo.popErrors()
  }

  private def applyFixmes(
      errors: List[TypeError],
      invalids: List[Invalid],
      elpMetadaOpt: Option[ElpMetadata],
  ): (List[TypeError], List[Invalid], List[RedundantFixme]) =
    elpMetadaOpt match {
      case None =>
        (errors, invalids, Nil)
      case Some(ElpMetadata(fixmes)) =>
        var usedFixmes = Set[Fixme]()
        val errors1 = ListBuffer[TypeError]()

        for (error <- errors) {
          findFixme(error.pos, fixmes) match {
            case Some(fixme) =>
              usedFixmes += fixme
            case None =>
              errors1 += error
          }
        }

        val invalids1 = ListBuffer[Invalid]()
        for (invalid <- invalids) {
          findFixme(invalid.pos, fixmes) match {
            case Some(fixme) =>
              usedFixmes += fixme
            case None =>
              invalids1 += invalid
          }
        }

        val redundantFixmePositions = fixmes.filterNot(usedFixmes).map(_.comment)
        val redundantFixmeErrors = redundantFixmePositions.map(RedundantFixme(_))

        (errors1.toList, invalids1.toList, redundantFixmeErrors)
    }

  private def findFixme(pos: Pos, fixmes: List[Fixme]): Option[Fixme] = pos match {
    case TextRange(startByte, _) =>
      fixmes.find(fixme => startByte >= fixme.suppression.startByte && startByte <= fixme.suppression.endByte)
    case _ =>
      None
  }
}
