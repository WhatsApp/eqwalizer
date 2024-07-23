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

libraryDependencies ++= Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.22.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.22.1" % "compile-internal"
)

assembly / assemblyJarName := "eqwalizer.jar"

run / javaOptions += "-Xss15M"

run / fork := true
run / connectInput := true

assembly / mainClass  := Some("com.whatsapp.eqwalizer.Main")
