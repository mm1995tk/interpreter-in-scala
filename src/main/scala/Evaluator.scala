package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.Token
import parser.{ParserError, ParserErrors}

object Evaluator:
  def apply(
      programOrErr: Either[ParserError | ParserErrors, Program]
  ): Either[ParserError | ParserErrors, Object] =
    programOrErr.map { _.reverse.headOption.map(eval).getOrElse(Object.Null) }

private def eval(stmt: Statement): Object = stmt match
  case Statement.Expr(expr) => evalExpr(expr)
  case _                    => ???

private def evalExpr(expr: Expr): Object = expr match
  case Expr.Int(Token.Int(v)) => (Object.Int(v))
  case Expr.Bool(t)           => (Object.Boolean(t.equals(Token.True)))
  case Expr.Prefix(t, expr) =>
    val right = evalExpr(expr)
    t match
      case Token.Minus => ???
      case Token.Bang  => ???
  case _ => ???

private val ConstNull = obj.Object.Null
