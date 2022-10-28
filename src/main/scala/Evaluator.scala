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
    programOrErr.map { _.reverse.headOption.map(eval).getOrElse(ConstNull) }

private def eval(stmt: Statement): Object = stmt match
  case Statement.Expr(expr) => evalExpr(expr)
  case _                    => ???

private def evalExpr(expr: Expr): Object = expr match
  case Expr.Int(Token.Int(v)) => Object.Int(v)
  case Expr.Bool(t)           => Object.Boolean(t.equals(Token.True))
  case expr: Expr.Prefix      => evalPrefixExpr(expr)
  case _                      => ???

private def evalPrefixExpr(item: Expr.Prefix) =
  val Expr.Prefix(t, expr) = item
  val right = evalExpr(expr)
  t match
    case Token.Minus =>
      right match
        case Object.Int(v) => Object.Int(-v)
        case _             => ConstNull
    case Token.Bang =>
      right match
        case Object.Int(v)     => Object.Boolean(false)
        case Object.Boolean(b) => Object.Boolean(!b)
        case _                 => Object.Boolean(true)
    case _ => ConstNull

private val ConstNull = obj.Object.Null
