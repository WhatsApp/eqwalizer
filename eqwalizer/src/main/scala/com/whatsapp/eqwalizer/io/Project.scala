/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.io

import com.whatsapp.eqwalizer.config

import java.io.File
import java.nio.file.Paths

object Project {

  val thirdPartySuffix = "/third-party"

  def relativePathOpt(file: String): Option[String] = {
    val projectRoot = Paths.get(config.sourceRoot)
    val otpRoot = Paths.get(config.otpLibRoot)
    val path = Paths.get(file)
    if (path.isAbsolute) {
      val canonical = Paths.get(new File(file).getCanonicalPath)
      if (canonical.startsWith(projectRoot)) {
        Some(projectRoot.relativize(canonical).toString)
      } else if (canonical.startsWith(otpRoot)) {
        Some(s"/otp/${otpRoot.relativize(canonical)}")
      } else if (canonical.toString().contains(thirdPartySuffix)) {
        // buck case, converts
        // /data/users/$user/whatsapp/server/buck-out/lsp/gen/waserver/f6717464dc059109/third-party/__proper__/proper/include/proper.hrl
        // into /third-party/__proper__/proper/include/proper.hrl
        val subPath = canonical.toString().split(thirdPartySuffix)(1)
        Some(s"$thirdPartySuffix$subPath")
      } else {
        // $COVERAGE-OFF$
        None
        // $COVERAGE-ON$
      }
    } else if (new File(config.sourceRoot, file).exists()) Some(file)
    // $COVERAGE-OFF$
    else None
  }
  // $COVERAGE-ON$

  def relativePath(file: String): String = {
    val path = Paths.get(file)
    relativePathOpt(file) match {
      case Some(path) => path
      case None       =>
        // $COVERAGE-OFF$
        if (path.isAbsolute) throw new IllegalStateException(s"cannot relativize the path: $file")
        // The only case we know of when paths will be unknown is when .erls are compiled with +deterministic.
        else s"<unknown>/$file"
      // $COVERAGE-ON$
    }
  }
}
