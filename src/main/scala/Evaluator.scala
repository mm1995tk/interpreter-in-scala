package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.Token

object Evaluator:
  def apply(program: Program): Option[Object] =
    program.reverse.headOption.flatMap(eval)

private def eval(stmt: Statement): Option[Object] = stmt match
  case Statement.Expr(expr) => evalExpr(expr)
  case _                    => None

private def evalExpr(expr: Expr): Option[Object] = expr match
  case Expr.Int(t)  => Some(Object.Int(t.value))
  case Expr.Bool(t) => Some(Object.Boolean(t.equals(Token.True)))
  case _            => None

private val ConstNull = obj.Object.Null
