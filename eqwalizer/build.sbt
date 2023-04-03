/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

name := "eqwalizer"

scalaVersion := "2.13.8"

scalacOptions += "-deprecation"
scalacOptions += "-Wunused:imports"
scalacOptions += "-Werror"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

Global / onChangedBuildSource := ReloadOnSourceChanges

target := {
  // see T102747790 - for setting custom build directory
  val defaultValue = target.value
  val useBuckOut = sys.env.getOrElse("EQWALIZER_USE_BUCK_OUT", "false").toBoolean
  if (useBuckOut)
    baseDirectory.value / ".." / ".." / ".." / ".." / "buck-out" / "eqwalizer"
  else
    defaultValue
}

libraryDependencies += "com.typesafe" % "config" % "1.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

libraryDependencies += "com.lihaoyi" %% "ujson" % "1.4.4"

assembly / assemblyJarName := "eqwalizer.jar"

assembly / test  := {}

lazy val testProjects = taskKey[Seq[File]]("build beam files")

Test / resourceGenerators += testProjects.taskValue

run / javaOptions += "-Xss15M"

run / fork := true
run / connectInput := true

Test / parallelExecution := false
Test / testOptions += Tests.Argument("-oD")

assembly / mainClass  := Some("com.whatsapp.eqwalizer.Main")

testProjects / fileInputs += (baseDirectory.value / "test_projects" / "*" / "src" / "*.erl").toGlob

testProjects := {
  import sys.process.Process
  def run(cmd: String): Unit = {
    streams.value.log.out(s"test_projects / $cmd")
    val cmdExitCode = Process(cmd, baseDirectory.value / "test_projects").!(pLog(streams.value.log))
    assert(cmdExitCode == 0, s"`$cmd` == 0")
  }
  run("rebar3 as test compile")
  run("rebar3 as test build_info --to ../test_projects.build_info")
  run("elp parse-all --project . --to .ast")
  val output = baseDirectory.value / "test_projects.build_info"
  Seq(output)
}

def pLog(log: Logger): scala.sys.process.ProcessLogger =
  new scala.sys.process.ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = log.err(s)
    def out(s: => String): Unit = log.out(s)
  }
