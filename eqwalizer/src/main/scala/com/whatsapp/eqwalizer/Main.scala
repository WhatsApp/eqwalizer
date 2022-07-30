/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.stub.DbApi
import com.whatsapp.eqwalizer.util.{ELPDiagnostics, StatDiagnostics, TcDiagnosticsText}

import java.io.OutputStream

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
      // $COVERAGE-OFF$
      !config.useIpc || cmd == "ipc",
      // $COVERAGE-ON$
      s"env var EQWALIZER_IPC=true is only valid with the 'ipc' command but got command $cmd",
    )

    cmd match {
      case "check"       => check(args1, json)
      case "ipc"         => ipc(args1)
      case "stats"       => StatDiagnostics.printStats()
      case "smoke"       => smokeRun()
      case "index"       => gleanIndex(args)
      case "custom-lint" => custom_lint()
      case _             => help()
    }
  }

  def check(checkArgs: Array[String], json: Boolean): Unit = {
    if (checkArgs.length != 2) {
      help()
      return
    }

    val module = checkArgs(1)
    DbApi.getAstStorage(module) match {
      case None =>
        throw new IllegalArgumentException(s"Cannot locate ast file for module `$module`")
      case Some(astStorage) =>
        val feedback =
          if (json)
            ELPDiagnostics.getDiagnosticsString(module, astStorage)
          else
            TcDiagnosticsText().checkFile(astStorage).mkString("", "\n", "\n")
        Console.println(feedback)
        Console.flush()
    }
  }

  def ipc(ipcArgs: Array[String]): Unit = {
    // $COVERAGE-OFF$ because covered by ELP tests
    if (!config.useIpc) {
      throw new IllegalArgumentException(s"expected env var USE_EQWALIZER_IPC=1 to be set")
    }
    val modules = ipcArgs.tail
    val modulesAndStorages = modules.distinct.flatMap(m => DbApi.getAstStorage(m).map(m -> _))
    ELPDiagnostics.getDiagnosticsIpc(modulesAndStorages)
    // $COVERAGE-ON$
  }

  private def smokeRun(): Unit = {
    // Writing to devNull is useful because it exercises error-message generation, can catch bugs like D32017880
    val devNull = new OutputStream() { override def write(b: Int): Unit = () }
    ELPDiagnostics.checkAll(devNull, showProgress = true)
    Console.flush()
  }

  private def custom_lint(): Unit = {
    val analysis = Class.forName("com.whatsapp.eqwalizer.custom.ApplicationEnv")
    val main = analysis.getDeclaredMethod("main")
    main.invoke(null)
    ()
  }

  private def gleanIndex(args: Array[String]): Unit = {
    val path = args.lift(1).getOrElse("")
    val indexerClass = Class.forName("com.whatsapp.eqwalizer.custom.GleanIndexer")
    val indexer = indexerClass.getConstructor(classOf[String]).newInstance(path)
    indexerClass.getDeclaredMethod("printIndex").invoke(indexer)
    ()
  }

  private def help(): Unit =
    Console.print(helpText)

  val helpText: String = {
    """com.whatsapp.eqwalizer.Main
      |usage:
      |    check <module_name>
      |    index <module_name> <path from repo root to Erlang source directory>
      |""".stripMargin
    /* undocumented:
     * `stats <module_name>` # stats for powering dashboards
     */
  }
}
