/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.*
import com.whatsapp.eqwalizer.ast.Forms.*
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid
import com.whatsapp.eqwalizer.ast.Types.{DynamicType, FunType}
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{
  NonexistentBehaviour,
  RedundantFixme,
  RedundantNowarnFunction,
  TypeError,
}
import com.whatsapp.eqwalizer.tc.{Options, PipelineContext, noOptions}

import scala.collection.mutable.ListBuffer

object Pipeline {
  def checkForms(moduleName: String, options: Options = noOptions): List[InternalForm] = {
    import scala.collection.mutable.ListBuffer

    val forms = Forms.load(moduleName)
    val module = forms.collectFirst { case Module(m) => m }.get
    val erlFile = forms.collectFirst { case File(f, _) => f }.get
    val noCheckFuns = forms.collect { case f: EqwalizerNowarnFunction => (f.id, f.pos) }.toMap
    val unlimitedRefinementFuns = forms.collect { case EqwalizerUnlimitedRefinement(id) => id }.toSet
    var currentFile = erlFile
    val result = ListBuffer.empty[InternalForm]
    for (form <- forms)
      form match {
        case f @ File(path, _) =>
          currentFile = path
          if (path == erlFile)
            result.addOne(f)
        case f: FunDecl if currentFile == erlFile =>
          val options1 =
            if (unlimitedRefinementFuns(f.id)) options.copy(unlimitedRefinement = Some(true)) else options
          val ctx = PipelineContext(module, options1)
          val checkedF = (Db.getSpec(module, f.id), Db.getOverloadedSpec(module, f.id)) match {
            case (Some(spec), _) =>
              checkFun(ctx, f, spec)
            case (_, Some(overloadedSpec)) =>
              checkOverloadedFun(ctx, f, overloadedSpec)
            case _ =>
              checkFun(ctx, f, getDynamicFunSpecType(f))
          }
          if (noCheckFuns.contains(f.id)) {
            result.addOne(applyNowarnToFun(f, checkedF, noCheckFuns(f.id)))
          } else {
            result.addOne(checkedF)
          }
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
            result.addOne(MisBehaviour(NonexistentBehaviour(b.pos, b.name))(b.pos))
          }
        case f if currentFile == erlFile =>
          result.addOne(f)
        case _ =>
        // skipping things from header files
      }
    result.toList
  }

  private def getDynamicFunSpecType(f: FunDecl): FunSpec =
    FunSpec(f.id, FunType(Nil, List.fill(f.id.arity)(DynamicType), DynamicType))(f.pos)

  private def checkFun(ctx: PipelineContext, f: FunDecl, spec: FunSpec): FuncDecl = {
    ctx.check.checkFun(f, spec)
    val errors = ctx.diagnosticsInfo.popErrors()
    FuncDecl(f.id, errors)(f.pos)
  }

  private def checkOverloadedFun(ctx: PipelineContext, f: FunDecl, overloadedSpec: OverloadedFunSpec): FuncDecl = {
    ctx.check.checkOverloadedFun(f, overloadedSpec)
    val errors = ctx.diagnosticsInfo.popErrors()
    FuncDecl(f.id, errors)(f.pos)
  }

  def applyFixmes(
      forms: List[InternalForm],
      invalids: List[Invalid],
      elpMetadaOpt: Option[ElpMetadata],
  ): (List[InternalForm], List[Invalid], List[RedundantFixme]) =
    elpMetadaOpt match {
      case None =>
        (forms, invalids, Nil)
      case Some(ElpMetadata(fixmes)) =>
        var usedFixmes = Set[Fixme]()
        val forms1 = ListBuffer[InternalForm]()

        for (form <- forms) {
          form match {
            case MisBehaviour(te) =>
              findFixme(te.pos, fixmes) match {
                case Some(fixme) =>
                  usedFixmes += fixme
                case None =>
                  forms1 += form
              }
            case decl @ FuncDecl(_, errors) =>
              val errors1 = ListBuffer[TypeError]()
              for (error <- errors) {
                findFixme(error.pos, fixmes) match {
                  case Some(fixme) =>
                    usedFixmes += fixme
                  case None =>
                    errors1 += error
                }
              }
              forms1 += decl.copy(errors = errors1.toList)(decl.pos)
            case form =>
              forms1 += form
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

        (forms1.toList, invalids1.toList, redundantFixmeErrors)
    }

  private def findFixme(pos: Pos, fixmes: List[Fixme]): Option[Fixme] = pos match {
    case TextRange(startByte, _) =>
      fixmes.find(fixme => startByte >= fixme.suppression.startByte && startByte <= fixme.suppression.endByte)
    case _ =>
      None
  }

  private def applyNowarnToFun(
      originalForm: FunDecl,
      checkedForm: InternalForm,
      pos: Pos,
  ): InternalForm = {
    val noErrors = checkedForm match {
      case FuncDecl(_, errors) =>
        errors.isEmpty
      case _ =>
        true
    }
    if (noErrors) {
      return FuncDecl(originalForm.id, errors = List(RedundantNowarnFunction(pos)))(originalForm.pos)
    }
    originalForm
  }
}
