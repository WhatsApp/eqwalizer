/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang.OtpInputStream
import com.whatsapp.eqwalizer.io.BuildInfo.AppInfo
import com.whatsapp.eqwalizer.io.EData._

import java.nio.file.{Files, Paths}

case class BuildInfo(otpLibRoot: String, sourceRoot: String, apps: Map[String, AppInfo], deps: Map[String, AppInfo])

object BuildInfo {
  case class AppInfo(name: String, dir: String, srcDirs: List[String], extraSrcDirs: List[String], ebin: String)

  def load(path: String): BuildInfo = {
    val bytes = Files.readAllBytes(Paths.get(path))
    val otpObject = new OtpInputStream(bytes).read_any()
    val buildInfoEtf = fromJava(otpObject)
    fromETF(buildInfoEtf)
  }

  private def fromETF(etf: EObject): BuildInfo = {
    val map = etf.asInstanceOf[EMap].entries.toMap
    val otpLibRoot = new String(map(EAtom("otp_lib_dir")).asInstanceOf[EBitStr].bin)
    val sourceRoot = new String(map(EAtom("source_root")).asInstanceOf[EBitStr].bin)
    val appKVs = map(EAtom("apps")).asInstanceOf[EList].elems
    val apps = appKVs.map(appInfo).map(ai => ai.name -> ai).toMap
    val depKVs = map(EAtom("deps")).asInstanceOf[EList].elems
    val deps = depKVs.map(appInfo).map(ai => ai.name -> ai).toMap
    BuildInfo(otpLibRoot, sourceRoot, apps, deps)
  }

  private def appInfo(obj: EObject): AppInfo = {
    val appMap = obj.asInstanceOf[EMap].entries.toMap
    val name = new String(appMap(EAtom("name")).asInstanceOf[EBitStr].bin)
    val ebin = new String(appMap(EAtom("ebin")).asInstanceOf[EBitStr].bin)
    val dir = new String(appMap(EAtom("dir")).asInstanceOf[EBitStr].bin)
    val srcDirs =
      appMap(EAtom("src_dirs")).asInstanceOf[EList].elems.map(d => new String(d.asInstanceOf[EBitStr].bin))
    val extraSrcDirs =
      appMap(EAtom("extra_src_dirs")).asInstanceOf[EList].elems.map(d => new String(d.asInstanceOf[EBitStr].bin))
    AppInfo(name, dir, srcDirs = srcDirs, extraSrcDirs = extraSrcDirs, ebin)
  }
}
