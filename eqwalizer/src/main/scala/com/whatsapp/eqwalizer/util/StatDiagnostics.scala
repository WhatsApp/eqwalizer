/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.{Pipeline, config}
import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.ExternalTypes.ExtType
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{AstListener, ExternalTypes, Forms, Id, RemoteId}
import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.tc.Options

import java.nio.file.{Files, Paths}

object StatDiagnostics {
  case class Stats(
      funCnt: Int,
      speccedCnt: Int,
      exportedCnt: Int,
      wellTypedCnt: Int,
      wellTypedSpeccedCnt: Int,
      wellTypedExportedCnt: Int,
      wellTypedExportedSpeccedCnt: Int,
      isGenerated: Boolean,
      eqwalizerEnabled: Boolean,
      errorCount: Int,
      fixmeCount: Int,
      isTest: Boolean,
      typeDependencies: List[String],
  ) {
    require(speccedCnt <= funCnt)
  }

  private type ModuleName = String
  private val projectModules = DbApi.projectApps.values.flatMap(_.modules).toSet

  def printStats(): Unit = {
    val moduleToStats = getModuleToStats()
    toJsonObj(moduleToStats).writeBytesTo(Console.out, indent = 2)
  }

  private def getModuleToStats(): Map[ModuleName, Stats] = {
    val apps = DbApi.projectApps.filter { case (appName, _) =>
      !config.apps(appName).dir.contains("third-party")
    }
    val modules = apps.values.flatMap(_.modules).toList.sorted
    modules.map(module => module -> toStats(module)).toMap
  }

  private def toStats(module: String): Stats = {
    val l = analyzeModule(module)
    val errorCountsByFunRid = getErrorCountsByFunRid(module, l.funs)
    val wellTyped = errorCountsByFunRid.filter(_._2 == 0).keySet
    val invalidCount = DbApi.getInvalidForms(module).get.size
    val errorCount = errorCountsByFunRid.values.sum + invalidCount

    val exported = if (l.exportAll) {
      l.funs
    } else {
      l.exports
    }

    Stats(
      funCnt = l.funs.size,
      speccedCnt = l.specced.size,
      exportedCnt = l.exports.size,
      wellTypedCnt = wellTyped.size,
      wellTypedSpeccedCnt = (wellTyped & l.specced).size,
      wellTypedExportedCnt = (wellTyped & exported).size,
      wellTypedExportedSpeccedCnt = (wellTyped & l.specced & exported).size,
      isGenerated = l.isGenerated,
      eqwalizerEnabled = l.eqwalizerEnabled,
      errorCount,
      fixmeCount = l.fixmeCount,
      isTest = l.isTest,
      typeDependencies = l.typeDependencies.toList.sorted,
    )
  }

  private def rid(module: String, id: Id): RemoteId = RemoteId(module, id.name, id.arity)

  private def toJsonObj(moduleToStats: Map[ModuleName, Stats]): ujson.Obj =
    ujson.Obj.from(moduleToStats.view.mapValues { s: Stats =>
      ujson.Obj(
        "fun_cnt" -> ujson.Num(s.funCnt),
        "exported_cnt" -> ujson.Num(s.exportedCnt),
        "well_typed_cnt" -> ujson.Num(s.wellTypedCnt),
        "specced_cnt" -> ujson.Num(s.speccedCnt),
        "well_typed_specced_cnt" -> ujson.Num(s.wellTypedSpeccedCnt),
        "well_typed_exported_cnt" -> ujson.Num(s.wellTypedExportedCnt),
        "well_typed_exported_specced_cnt" -> ujson.Num(s.wellTypedExportedSpeccedCnt),
        "fixme_count" -> ujson.Num(s.fixmeCount),
        "is_generated" -> ujson.Bool(s.isGenerated),
        "eqwalizer_enabled" -> ujson.Bool(s.eqwalizerEnabled),
        "error_count" -> ujson.Num(s.errorCount),
        "is_test" -> ujson.Bool(s.isTest),
        "type_dependencies" -> s.typeDependencies,
      )
    })

  private def getErrorCountsByFunRid(module: String, rids: Set[RemoteId]): Map[RemoteId, Int] = {
    val ids = rids.map(rid => Id(rid.name, rid.arity))
    val astStorage = DbApi.getAstStorage(module).get
    val options = Options(
      tolerateErrors = Some(true),
      gradualTyping = Some(true),
      eqwater = Some(true),
    )
    Pipeline.checkFunsAndApplyFixmes(astStorage, ids, options)
  }

  private def analyzeModule(module: String): Listener = {
    val astStorage = DbApi.getAstStorage(module).get
    val forms = Forms.load(astStorage)
    val fixmeCount = forms.collectFirst { case elpMetadata: ElpMetadata => elpMetadata.fixmes.size }.getOrElse(0)
    val listener = new Listener(module, fixmeCount)
    Pipeline.traverseForms(forms, listener)
    listener
  }

  class Listener(module: String, val fixmeCount: Int) extends AstListener {
    var exports = Set[RemoteId]()
    var funs = Set[RemoteId]()
    var specced = Set[RemoteId]()
    var exportAll = false
    var isGenerated = false
    var eqwalizerEnabled = false
    var isTest = false
    private val generatedMark: String = "@" + "generated"
    var typeDependencies = Set[String]()

    private def addDep(rid: RemoteId): Unit =
      if (rid.module != module && projectModules(rid.module)) {
        typeDependencies += rid.module
      }

    override def enterModule(m: String, erlFile: String): Unit = {
      val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(200)
      isGenerated = preamble.contains(generatedMark)
      isTest = erlFile.contains("/test/")
      if (!isTest && !isGenerated && DbApi.getModuleApp(module).get.hasEqwalizerMarker) {
        eqwalizerEnabled = true
      }
    }

    override def enterExport(e: Export): Unit =
      exports ++= e.funs.map(rid(module, _)).toSet

    override def enterCompileExportAll(form: CompileExportAll): Unit =
      exportAll = true

    override def enterFunSpec(spec: ExternalFunSpec): Unit =
      specced += rid(module, spec.id)

    override def enterFunDecl(decl: FunDecl): Unit =
      funs += rid(module, decl.id)

    override def enterTypingAttribute(typing: TypingAttribute): Unit =
      if (typing.names.contains("eqwalizer")) eqwalizerEnabled = true

    override def enterExpr(e: Expr): Unit = e match {
      case RemoteCall(rid, _args) =>
        addDep(rid)
      case RemoteFun(rid) =>
        addDep(rid)
      case _ => ()
    }

    override def enterType(ty: ExtType): Unit = ty match {
      case ExternalTypes.RemoteExtType(rid, _) =>
        addDep(rid)
      case _ => ()
    }
  }
}
