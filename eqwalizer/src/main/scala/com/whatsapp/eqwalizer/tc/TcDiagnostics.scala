/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Pos
import com.whatsapp.eqwalizer.ast.Show.{show, showNotSubtype}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic

object TcDiagnostics {
  sealed trait TypeError extends Diagnostic
  case class ExpectedSubtype(pos: Pos, expr: Expr, expected: Type, got: Type)(implicit
      val pipelineContext: PipelineContext
  ) extends TypeError {
    private val (showGot, showExpected) = showNotSubtype(got, expected)
    override val msg: String =
      s"Expression has type:   $showGot\nContext expected type: $showExpected"
    private val typeMismatch = pipelineContext.typeMismatch

    def errorName = "incompatible_types"
    override def erroneousExpr: Option[Expr] = Some(expr)
    override lazy val explanation: Option[String] = {
      typeMismatch.explain(got, expected)
    }
  }
  case class ExpectedFunType(pos: Pos, expr: Expr, expectedArity: Int, got: Type)(implicit
      val
      pipelineContext: PipelineContext
  ) extends TypeError {
    val msg: String = s"Expected fun type with arity $expectedArity\nGot: ${show(got)}"
    def errorName = "expected_fun_type"
    override def erroneousExpr: Option[Expr] = Some(expr)
  }
  case class NoSpecialType(pos: Pos, expr: Expr, argTys: List[Type])(implicit val pipelineContext: PipelineContext)
      extends TypeError {
    private val argTysString = argTys.map(show).mkString(", ")
    override val msg: String = s"Not enough info to branch. Arg types: $argTysString"
    def errorName = "not_enough_info_to_branch"
    override def erroneousExpr: Option[Expr] = Some(expr)
  }
  case class DynamicLambda(pos: Pos) extends TypeError {
    override val msg: String = s"Lambda without context: parameters are dynamic()"
    def errorName = "dynamic_lambda"
    override def erroneousExpr: Option[Expr] = None
  }
  case class LambdaArityMismatch(pos: Pos, expr: Expr, lambdaArity: Int, argsArity: Int) extends TypeError {
    override val msg: String = s"fun with arity $lambdaArity used as fun with $argsArity arguments"
    def errorName = "fun_arity_mismatch"
    override def erroneousExpr: Option[Expr] = Some(expr)
  }
  case class IndexOutOfBounds(pos: Pos, expr: Expr, index: Int, tupleArity: Int) extends TypeError {
    override val msg: String = s"Tried to access element $index of a tuple with $tupleArity elements"
    def errorName = "index_out_of_bounds"
    override def erroneousExpr: Option[Expr] = Some(expr)
  }
  case class UndefinedField(pos: Pos, recName: String, fieldName: String) extends TypeError {
    override val msg: String = s"#$recName{...}: $fieldName is 'undefined'"
    def errorName = "undefined_field"
    override def erroneousExpr: Option[Expr] = None
  }
  case class UnboundVar(pos: Pos, n: String) extends TypeError {
    override val msg: String = s"Unbound var: ${n}"
    def errorName = "unbound_var"
    override def erroneousExpr: Option[Expr] = None
  }
  case class UnboundRecord(pos: Pos, rec: String) extends TypeError {
    override val msg: String = s"Unbound rec: ${rec}"
    def errorName = "unbound_record"
    override def erroneousExpr: Option[Expr] = None
  }
  case class NonexistentBehaviour(pos: Pos, name: String) extends TypeError {
    override val msg: String = s"Behaviour does not exist: $name"
    def errorName = "behaviour_does_not_exist"
    override def erroneousExpr: Option[Expr] = None
  }
  case class MissingCallback(pos: Pos, behaviourName: String, callback: String) extends TypeError {
    override val msg: String = s"Missing implementation for $behaviourName callback $callback"
    def errorName = "missing_cb_implementation"
    override def erroneousExpr: Option[Expr] = None
  }
  case class IncorrectCallbackReturn(pos: Pos, behaviourName: String, callback: String, expected: Type, got: Type)(
      implicit val pipelineContext: PipelineContext
  ) extends TypeError {
    override val msg: String =
      s"Incorrect return type for implementation of $behaviourName:$callback.\nExpected: ${show(expected)}\nGot:      ${show(got)}"

    override lazy val explanation: Option[String] = pipelineContext.typeMismatch.explain(got, expected)

    def errorName = "incorrect_return_type_in_cb_implementation"
    override def erroneousExpr: Option[Expr] = None
  }
  case class IncorrectCallbackParams(
      pos: Pos,
      behaviourName: String,
      callback: String,
      paramIndex: Int,
      expected: Type,
      got: Type,
  )(implicit val pipelineContext: PipelineContext)
      extends TypeError {
    override val msg: String =
      s"Parameter ${paramIndex + 1} in implementation of $behaviourName:$callback has no overlap with expected parameter type. Expected: ${show(expected)}, Got: ${show(got)}."
    def errorName = "incorrect_param_type_in_cb_implementation"
    override def erroneousExpr: Option[Expr] = None
  }
  case class UnhandledOp(pos: Pos, op: String) extends IllegalStateException(s"Position: $pos, Unhandled op: $op")
  case class RevealTypeHint(t: Type)(val pos: Pos)(implicit val pipelineContext: PipelineContext) extends TypeError {
    private val typeS = show(t)
    override val errorName = "reveal_type"
    override val msg = typeS
    override def erroneousExpr: Option[Expr] = None
  }
  case class RedundantFixme(pos: Pos) extends TypeError {
    override val msg: String = "redundant fixme"
    def errorName = "redundant_fixme"
    override def erroneousExpr: Option[Expr] = None
  }
  case class RedundantNowarnFunction(pos: Pos) extends TypeError {
    override val msg: String = "redundant nowarn_function"
    def errorName = "redundant_nowarn_function"
    override def erroneousExpr: Option[Expr] = None
  }
  case class AmbiguousUnion(pos: Pos, expr: Expr, expected: Type, got: Type)(implicit
      val pipelineContext: PipelineContext
  ) extends TypeError {
    override val msg: String =
      s"Expression has type ${show(got)} which matches multiple generic types in ${show(expected)}"
    def errorName = "ambiguous_union"
    override def erroneousExpr: Option[Expr] = Some(expr)
  }
  case class ClauseNotCovered(pos: Pos) extends TypeError {
    override val msg: String = "Clause is not covered by spec"
    val errorName = "clause_not_covered"
    override val erroneousExpr: Option[Expr] = None
  }
  case class AmbiguousLambda(pos: Pos, expr: Expr, expected: Type)(implicit
      val pipelineContext: PipelineContext
  ) extends TypeError {
    override val msg: String =
      s"Functional expression can match multiple types in ${show(expected)}"

    def errorName = "ambiguous_lambda"

    override def erroneousExpr: Option[Expr] = Some(expr)
  }
}
