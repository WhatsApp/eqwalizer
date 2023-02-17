package com.whatsapp.eqwalizer.ast

object CompilerMacro {
  val fake_module = "$compiler_macro"

  val funs: Set[Id] = Set(
    Id("record_info", 2)
  )
}
