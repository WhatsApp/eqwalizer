/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.util.ELPDiagnostics

object Main {
  def main(args: Array[String]): Unit = {
    val json = args.contains("--json")
    val args1 = args.filterNot(Set("--no-progress", "--json"))
    if (args1.length == 0) {
      help()
      return
    }

    val cmd = args1(0)
    require(
      !(config.mode == Mode.Standalone && cmd == "ipc"),
      s"'ipc' command can only be used by running eqWAlizer through ELP (got command $cmd)",
    )

    cmd match {
      case "ipc" => ipc(args1)
      case _     => help()
    }
  }

  def ipc(ipcArgs: Array[String]): Unit = {
    if (config.mode == Mode.Standalone) {
      throw new IllegalArgumentException(s"eqWAlizer should be called from ELP to use IPC")
    }
    val modules = ipcArgs.tail
    val modulesAndStorages = modules.distinct.flatMap(m => DbApi.getAstStorage(m).map(m -> _))
    ELPDiagnostics.getDiagnosticsIpc(modulesAndStorages)
  }

  private def help(): Unit =
    Console.print(helpText)

  val helpText: String = {
    """com.whatsapp.eqwalizer.Main
      |usage:
      |    check <module_name>
      |""".stripMargin
    /* undocumented:
     * `stats <module_name>` # stats for powering dashboards
     */
  }
}
