/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.whatsapp.eqwalizer.ast.{App, ConvertAst, ExtModuleStub, Id}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
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

  private def loadExtModuleStub(forms: List[ExternalForm], module: String): ExtModuleStub = {
    ExtModuleStub(
      module,
      forms,
      Set.empty,
    )
  }

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

  private var rawModuleStubs: Map[String, ExtModuleStub] =
    Map.empty
  private var expandedModuleStubs: Map[String, ModuleStub] =
    Map.empty
  private var contractiveModuleStubs: Map[String, ModuleStub] =
    Map.empty
  private var validatedModuleStubs: Map[String, ModuleStub] =
    Map.empty
  private var transValidStubs: Map[String, ModuleStub] =
    Map.empty

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

  def getRawModuleStub(module: String): Option[ExtModuleStub] = {
    if (rawModuleStubs.contains(module))
      Some(rawModuleStubs(module))
    else
      getExtModuleStub(module).map { s =>
        var types: Set[Id] = Set.empty
        s.forms.foreach {
          case f: ExternalTypeDecl =>
            types += f.id
          case f: ExternalOpaqueDecl =>
            types += f.id
          case _ =>
            ()
        }
        val stub = s.copy(types = types)
        rawModuleStubs = rawModuleStubs.updated(module, stub)
        stub
      }
  }

  def getExpandedModuleStub(module: String): Option[ModuleStub] = {
    if (expandedModuleStubs.contains(module))
      Some(expandedModuleStubs(module))
    else
      getRawModuleStub(module).map { s =>
        val stub = Expander.expandStub(s)
        expandedModuleStubs = expandedModuleStubs.updated(module, stub)
        stub
      }
  }

  def getContractiveModuleStub(module: String): Option[ModuleStub] = {
    if (contractiveModuleStubs.contains(module))
      Some(contractiveModuleStubs(module))
    else
      getExpandedModuleStub(module).map { s =>
        val contractivity = new Contractivity(module)
        val stub = contractivity.checkStub(s)
        contractiveModuleStubs = contractiveModuleStubs.updated(module, stub)
        stub
      }
  }

  def getValidatedModuleStub(module: String): Option[ModuleStub] = {
    if (validatedModuleStubs.contains(module))
      Some(validatedModuleStubs(module))
    else
      getContractiveModuleStub(module).map { s =>
        val typesValid = new TypesValid()
        val stub = typesValid.checkStub(s)
        validatedModuleStubs = validatedModuleStubs.updated(module, stub)
        stub
      }
  }

  /** module stub suitable for type-checking
    */
  def getModuleStub(module: String): Option[ModuleStub] =
    if (transValidStubs.contains(module))
      Some(transValidStubs(module))
    else
      getValidatedModuleStub(module).map { s =>
        val stub = new TransValid().checkStub(s)
        transValidStubs = transValidStubs.updated(module, stub)
        stub
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
