/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import com.whatsapp.eqwalizer.ast.{App, ConvertAst, ExtModuleStub, Id}
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.config
import com.whatsapp.eqwalizer.io.BuildInfo.AppInfo
import com.whatsapp.eqwalizer.io.{AstLoader, EData}

private object Db {
  private def beamModules(dir: String): List[String] =
    Paths.get(dir).toFile.listFiles((_, f) => f.endsWith(".beam")).map(_.getName.dropRight(5)).toList

  private def erlModules(appInfo: AppInfo): List[String] =
    // matching rebar3 and ELP behavior, srcDirs is searched recursively but extraSrcDirs is not
    (dirsToModules(appInfo.dir, appInfo.srcDirs, maxDepth = Int.MaxValue) ++
      dirsToModules(appInfo.dir, appInfo.extraSrcDirs, maxDepth = 1)).distinct

  private lazy val otpEbinDirs: Map[String, String] = {
    val libRoot = config.otpLibRoot
    val libs = Paths.get(libRoot).toFile.listFiles().filter(_.isDirectory).map(_.getName)
    libs.map(dir => dir.split("-")(0) -> s"$libRoot/$dir/ebin").toMap
  }

  lazy val otpApps: Map[String, App] =
    otpEbinDirs.map { case (n, dir) => n -> App(n, dir, beamModules(dir), hasEqwalizerMarker = false) }
  lazy val otpModules: Set[String] =
    otpApps.values.flatMap(_.modules).toSet
  lazy val projectApps: Map[String, App] =
    config.apps.map { case (n, ai) => n -> App(n, ai.ebin, erlModules(ai), hasEqwalizerMarker(ai)) }
  lazy val projectModules: Set[String] =
    projectApps.values.flatMap(_.modules).toSet
  lazy val depApps: Map[String, App] =
    config.deps.map { case (n, ai) => n -> App(n, ai.ebin, erlModules(ai), hasEqwalizerMarker = false) }
  lazy val depModules: Set[String] =
    depApps.values.flatMap(_.modules).toSet
  lazy val apps: Map[String, App] =
    otpApps ++ projectApps ++ depApps

  private lazy val module2App: Map[String, Set[String]] = {
    var result = Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    for {
      (_, App(appName, _, modules, _)) <- apps
      module <- modules
    } result = result.updated(module, result(module) + appName)
    result
  }

  private def loadExtModuleStub(forms: List[ExternalForm], module: String): ExtModuleStub =
    ExtModuleStub(module, forms)

  def loadStubForms(module: String): Option[List[ExternalForm]] = {
    getAstStorage(module).map { astStorage =>
      val formsJ = AstLoader.loadAbstractFormsJ(astStorage)(stubsOnly = true)
      val formsDef = (for {
        i <- 0 until formsJ.arity()
        f = formsJ.elementAt(i)
        if !isFunForm(f)
      } yield f).toArray
      val fromBeam = Db.fromBeam(module)
      formsDef.flatMap(f => new ConvertAst(fromBeam).convertForm(EData.fromJava(f))).toList
    }
  }

  private val rawModuleStubs: mutable.Map[String, ExtModuleStub] =
    mutable.Map.empty
  private val typeIds: mutable.Map[String, Set[Id]] =
    mutable.Map.empty
  private val expandedModuleStubs: mutable.Map[String, Option[ModuleStub]] =
    mutable.Map.empty
  private val contractiveModuleStubs: mutable.Map[String, Option[ModuleStub]] =
    mutable.Map.empty
  private val validatedModuleStubs: mutable.Map[String, Option[ModuleStub]] =
    mutable.Map.empty
  private val transValidStubs: mutable.Map[String, Option[ModuleStub]] =
    mutable.Map.empty

  def getModuleApp(module: String): Option[App] = {
    val appNames = module2App(module)
    appNames.headOption.flatMap(apps.get)
  }

  def getExtModuleStub(module: String): Option[ExtModuleStub] =
    loadStubForms(module).map(loadExtModuleStub(_, module))

  def getAstStorage(module: String): Option[DbApi.AstStorage] =
    getModuleApp(module).map { app =>
      if (fromBeam(module)) {
        val path = Paths.get(app.ebinDir, s"$module.beam")
        DbApi.AstBeam(path)
      } else if (config.useIpc) {
        DbApi.AstEtfIpc(module)
      } else {
        val etf = Paths.get(config.astDir.get, s"$module.etf")
        DbApi.AstEtfFile(path = etf)
      }
    }

  // Internal API for Includes analysis - T111602174
  def getBeamAstStorage(module: String): Option[DbApi.AstBeam] =
    getModuleApp(module).flatMap { app =>
      val path = Paths.get(app.ebinDir, s"$module.beam")
      // We don't know where beams for test modules are, so skip them
      if (Files.exists(path)) Some(DbApi.AstBeam(path))
      else None
    }

  private def getRawModuleStub(module: String): Option[ExtModuleStub] =
    if (typeIds.contains(module))
      rawModuleStubs.get(module)
    else {
      val res = getExtModuleStub(module) match {
        case Some(stub) =>
          var mTypeIds: Set[Id] = Set.empty
          stub.forms.foreach {
            case f: ExternalTypeDecl =>
              mTypeIds += f.id
            case f: ExternalOpaqueDecl =>
              mTypeIds += f.id
            case _ =>
              ()
          }
          typeIds.put(module, mTypeIds)
          rawModuleStubs.put(module, stub)
          Some(stub)
        case None =>
          typeIds.put(module, Set.empty)
          None
      }
      res
    }

  def getTypeIds(module: String): Set[Id] =
    typeIds.get(module) match {
      case Some(ids) =>
        ids
      case None =>
        getRawModuleStub(module)
        typeIds(module)
    }

  def getExpandedModuleStub(module: String): Option[ModuleStub] =
    if (expandedModuleStubs.contains(module))
      expandedModuleStubs(module)
    else {
      val optStub = getRawModuleStub(module).map(Expander.expandStub)
      expandedModuleStubs.put(module, optStub)
      rawModuleStubs.remove(module)
      optStub
    }

  def getContractiveModuleStub(module: String): Option[ModuleStub] =
    if (contractiveModuleStubs.contains(module))
      contractiveModuleStubs(module)
    else {
      val optStub = getExpandedModuleStub(module).map { new Contractivity(module).checkStub }
      contractiveModuleStubs.put(module, optStub)
      optStub
    }

  def getValidatedModuleStub(module: String): Option[ModuleStub] =
    if (validatedModuleStubs.contains(module))
      validatedModuleStubs(module)
    else {
      val optStub = getContractiveModuleStub(module).map { new TypesValid().checkStub }
      validatedModuleStubs.put(module, optStub)
      optStub
    }

  /** module stub suitable for type-checking
    */
  def getModuleStub(module: String): Option[ModuleStub] =
    if (transValidStubs.contains(module))
      transValidStubs(module)
    else {
      val optStub = getValidatedModuleStub(module).map { new TransValid().checkStub }
      transValidStubs.put(module, optStub)
      optStub
    }

  def fromBeam(module: String): Boolean =
    ((otpModules(module) || depModules(module)) && !projectModules(module)) || !config.useElp

  private val generatedMark: String = "@" + "generated"
  def isGenerated(module: String): Boolean = {
    val astStorage = getAstStorage(module).get
    val formsJ = AstLoader.loadAbstractFormsJ(astStorage)
    val fromBeam = Db.fromBeam(module)
    for {
      i <- 0 until formsJ.arity()
      form <- new ConvertAst(fromBeam).convertForm(EData.fromJava(formsJ.elementAt(i)))
    } form match {
      case File(erlFile, _) =>
        val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(200)
        return preamble.contains(generatedMark)
      case _ =>
    }
    false
  }

  def isGpbCompileGenerated(module: String): Boolean = {
    if (!module.contains("pb"))
      return false
    val astStorage = getAstStorage(module).get
    val formsJ = AstLoader.loadAbstractFormsJ(astStorage)
    for {
      i <- 0 until formsJ.arity()
      form <- new ConvertAst(fromBeam(module)).convertForm(EData.fromJava(formsJ.elementAt(i)))
    } form match {
      case File(erlFile, _) =>
        val preamble = new String(Files.readAllBytes(Paths.get(erlFile))).take(1000)
        return preamble.contains("Generated by gpb_compile")
      case _ =>
    }
    false
  }

  private def hasEqwalizerMarker(appInfo: AppInfo): Boolean =
    Files.exists(Paths.get(s"${appInfo.dir}/.eqwalizer"))

  private def dirsToModules(root: String, dirs: List[String], maxDepth: Int): List[String] =
    dirs
      .map(srcDir => Paths.get(s"$root/$srcDir"))
      .filter(Files.exists(_))
      .flatMap { srcPath =>
        val erlPaths = Files
          .find(
            srcPath,
            maxDepth,
            (path, attrs) => attrs.isRegularFile && path.getFileName.toString.endsWith(".erl"),
          )
          .iterator()
          .asScala
          .toList
        erlPaths.map(_.getFileName.toString.dropRight(4))
      }
}
