/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.util.ELPDiagnostics

object Main {
  def main(args: Array[String]): Unit = {
    val args1 = args.filterNot(Set("--no-progress"))
    if (args1.length == 0) {
      help()
      return
    }

    val cmd = args1(0)

    cmd match {
      case "ipc" => ipc(args1)
      case _     => help()
    }
  }

  def ipc(ipcArgs: Array[String]): Unit = {
    val modules = ipcArgs.tail
    ELPDiagnostics.getDiagnosticsIpc(modules)
  }

  private def help(): Unit =
    Console.print(helpText)

  val helpText: String = {
    """com.whatsapp.eqwalizer.Main
      |eqWAlizer is meant to be used from ELP
      |""".stripMargin
    /* undocumented:
     * `stats <module_name>` # stats for powering dashboards
     */
  }
}
