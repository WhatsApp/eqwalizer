/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

name := "eqwalizer"

scalaVersion := "3.6.4"

scalacOptions += "-deprecation"
scalacOptions += "-Wunused:imports"
scalacOptions += "-Werror"

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

libraryDependencies += "com.typesafe" % "config" % "1.4.3"

libraryDependencies ++= Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.33.3",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.33.3" % "compile-internal"
)

initialize := {
  // Ensure previous initializations are run
  val _ = initialize.value

  // Retrieve the JVM's class version and specification version
  val classVersion = sys.props("java.class.version")
  val specVersion = sys.props("java.specification.version")

  // Assert that the JVM meets the minimum required version, for example, Java 17
  assert(specVersion.toDouble >= 11, "Java 11 or above is required to run this project.")
}

assembly / assemblyJarName := "eqwalizer.jar"

assembly / mainClass  := Some("com.whatsapp.eqwalizer.Main")
