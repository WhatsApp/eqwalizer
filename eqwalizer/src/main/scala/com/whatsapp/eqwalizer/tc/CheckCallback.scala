/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

class CheckCallback(pipelineContext: PipelineContext) {
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def checkImpl(module: String, b: Behaviour, cb: Callback, isOptional: Boolean): Option[InternalForm] =
    if (Db.isExported(module, cb.id)) {
      Db.getSpec(module, cb.id) match {
        case Some(FunSpec(_, impl)) =>
          // don't validate invalid callbacks (callback validation is defeasible)
          if (cb.tys.isEmpty) return None
          val expectedResTy = subtype.join(cb.tys.map(_.resTy))
          if (!subtype.subType(impl.resTy, expectedResTy)) {
            val te = IncorrectCallbackReturn(b.name, cb.id.toString, expectedResTy, impl.resTy)(b.pos)
            return Some(MisBehaviour(te)(b.pos))
          }

          val badParamOpt = impl.argTys.zipWithIndex.find { case (implArgTy, index) =>
            !cb.tys.exists { case FunType(_, cbArgTys, _) =>
              val cbArgTy = cbArgTys(index)
              val approxMeet = narrow.meet(implArgTy, cbArgTy)
              val hasOverlap =
                subtype.isNoneType(implArgTy) || subtype.isNoneType(cbArgTy) || !subtype.isNoneType(approxMeet)
              hasOverlap
            }
          }
          badParamOpt match {
            case Some((implArgTy, paramIndex)) =>
              val exp = subtype.join(cb.tys.map(_.argTys(paramIndex)))
              val te =
                IncorrectCallbackParams(b.name, cb.id.toString, paramIndex, expected = exp, got = implArgTy)(b.pos)
              Some(MisBehaviour(te)(b.pos))
            case None =>
              None
          }
        case None =>
          // allow unspecced behaviour implementations
          None
      }
    } else {
      if (isOptional) None
      else {
        val te = MissingCallback(b.name, cb.id.toString)(b.pos)
        Some(MisBehaviour(te)(b.pos))
      }
    }
}
