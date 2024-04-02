/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types.{DynamicType, FunType}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.TcDiagnostics.{
  NonexistentBehaviour,
  RedundantFixme,
  RedundantNowarnFunction,
  TypeError,
}
import com.whatsapp.eqwalizer.tc.{Options, PipelineContext, noOptions}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Pipeline {
  def checkForms(astStorage: DbApi.AstStorage, options: Options = noOptions): List[InternalForm] = {
    import scala.collection.mutable.ListBuffer

    val forms = Forms.load(astStorage).collect { case f: InternalForm => f }
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
          val checkedF = (DbApi.getSpec(module, f.id), DbApi.getOverloadedSpec(module, f.id)) match {
            case (Some(spec), _) =>
              checkFun(ctx, f, spec)
            case (_, Some(overloadedSpec)) =>
              checkOverloadedFun(ctx, f, overloadedSpec)
            case _ =>
              if (ctx.gradualTyping)
                checkFun(ctx, f, getDynamicFunSpecType(f))
              else
                NoSpecFuncDecl(f.id)(f.pos)
          }
          if (noCheckFuns.contains(f.id)) {
            result.addOne(applyNowarnToFun(f, checkedF, noCheckFuns(f.id)))
          } else {
            result.addOne(checkedF)
          }
        case b: Behaviour =>
          val ctx = PipelineContext(module, options)
          if (DbApi.isKnownModule(b.name)) {
            val (callbacks, optional) = DbApi.getCallbacks(b.name)
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

  def traverseForms(forms: List[ExternalForm], listener: AstListener): Unit = {
    val traverse = new Traverse(listener)
    traverse.traverseForms(forms)
  }

  private def getDynamicFunSpecType(f: FunDecl): FunSpec =
    FunSpec(f.id, FunType(Nil, List.fill(f.id.arity)(DynamicType), DynamicType))(f.pos)

  private def checkFun(ctx: PipelineContext, f: FunDecl, spec: FunSpec): FuncDecl = {
    if (ctx.tolerateErrors)
      tolerantCheckFun(ctx, f, spec, ListBuffer.empty)
    else
      try {
        ctx.check.checkFun(f, spec)
        val errors = ctx.diagnosticsInfo.popErrors()
        FuncDecl(f.id, errors)(f.pos)
      } catch {
        case te: TypeError =>
          FuncDecl(f.id, errors = List(te))(f.pos)
      }
  }

  @tailrec
  private def tolerantCheckFun(
      ctx: PipelineContext,
      f: FunDecl,
      spec: FunSpec,
      typeErrors: ListBuffer[TypeError],
  ): FuncDecl = {
    val patched =
      try {
        ctx.check.checkFun(f, spec)
        return FuncDecl(f.id, typeErrors.toList)(f.pos)
      } catch {
        case te: TypeError =>
          typeErrors.addOne(te)
          te.erroneousExpr match {
            case Some(expr) if typeErrors.size < 5 =>
              new Patch(expr).patchFun(f)
            case _ =>
              return FuncDecl(f.id, typeErrors.toList)(f.pos)
          }
      }
    ctx.typeInfo.clear(f.pos)
    tolerantCheckFun(ctx, patched, spec, typeErrors)
  }

  private def checkOverloadedFun(ctx: PipelineContext, f: FunDecl, overloadedSpec: OverloadedFunSpec): FuncDecl = {
    try {
      ctx.check.checkOverloadedFun(f, overloadedSpec)
      val errors = ctx.diagnosticsInfo.popErrors()
      FuncDecl(f.id, errors)(f.pos)
    } catch {
      case te: TypeError =>
        FuncDecl(f.id, errors = List(te))(f.pos)
    }
  }

  def applyFixmes(
      forms: List[InternalForm],
      elpMetadaOpt: Option[ElpMetadata],
  ): (List[InternalForm], List[RedundantFixme]) =
    elpMetadaOpt match {
      case None =>
        (forms, Nil)
      case Some(ElpMetadata(fixmes)) =>
        var usedFixmes = Set[Fixme]()
        val forms1 = ListBuffer[InternalForm]()

        for (form <- forms) {
          form match {
            case invalid: InvalidForm =>
              findFixme(invalid.te.pos, fixmes) match {
                case Some(fixme) =>
                  usedFixmes += fixme
                case None =>
                  forms1 += invalid
              }
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

        val redundantFixmePositions = fixmes.filterNot(usedFixmes).map(_.comment)
        val redundantFixmeErrors = redundantFixmePositions.map(RedundantFixme)

        (forms1.toList, redundantFixmeErrors)
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
