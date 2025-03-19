/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.whatsapp.eqwalizer.ast.Types.Type
import com.whatsapp.eqwalizer.ast.{Pos, TextRange}
import com.whatsapp.eqwalizer.{Mode, config}

import scala.collection.mutable

class TypeInfo(pipelineContext: PipelineContext) {
  private lazy val subtype = pipelineContext.subtype
  private val moduleTypeInfo = TypeInfo.info.getOrElseUpdate(pipelineContext.module, mutable.Map.empty)
  private var collect: Int = 1
  private var collectLambdas: Int = 1

  def add(pos: Pos, resTy: Type): Unit = {
    if (config.mode == Mode.ElpIde && collect > 0) {
      moduleTypeInfo.updateWith(pos) {
        case Some(ty) => Some(subtype.join(ty, resTy))
        case None     => Some(resTy)
      }
    }
  }

  private def setCollect(c: Boolean): Unit =
    if (c)
      collect += 1
    else
      collect -= 1

  private def setCollectLambdas(c: Boolean): Unit =
    if (c)
      collectLambdas += 1
    else
      collectLambdas -= 1

  private def isCollectLambdas: Boolean =
    collectLambdas > 0

  def isCollect: Boolean =
    collect > 0

  def withoutTypeCollection[Body](body: => Body): Body = {
    setCollect(false)
    val result = body
    setCollect(true)
    result
  }

  def withoutLambdaTypeCollection[Body](body: => Body): Body = {
    setCollectLambdas(false)
    val result = body
    setCollectLambdas(true)
    result
  }

  def processLambda[Body](body: => Body): Body = {
    if (!isCollectLambdas)
      setCollect(false)

    val result = body

    if (!isCollectLambdas)
      setCollect(true)

    result
  }

  def clear(pos: Pos): Unit = {
    pos match {
      case TextRange(startByte, endByte) =>
        moduleTypeInfo.filterInPlace {
          case (TextRange(startExpr, endExpr), _) =>
            startByte > startExpr || endExpr > endByte
          case _ => true
        }
      case _ => ()
    }
  }
}

object TypeInfo {
  private val info: mutable.Map[String, mutable.Map[Pos, Type]] = mutable.Map.empty

  def toJson: ujson.Value = {
    ujson.read(writeToString(info.map { case (module, types) => (module, types.toList) }))
  }

  implicit private val codec: JsonValueCodec[mutable.Map[String, List[(Pos, Type)]]] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true).withDiscriminatorFieldName(None).withFieldNameMapper {
      case "pos" => "location"
      case "mod" => "module"
      case s     => JsonCodecMaker.enforce_snake_case(s)
    }
  )
}
