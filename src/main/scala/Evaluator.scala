package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.Token
import parser.{ParserError, ParserErrors}
import java.util.Objects
import cats.Bifunctor

def evalProgram(program: Program): Either[EvalError, Object] =
  program.headOption match
    case None => Right(ConstNull)
    case Some(h) =>
      program.tail.foldLeft(evalStatement(h)) { (acc, cur) =>
        acc match
          case err @ Left(_)                     => err
          case rv @ Right(Object.ReturnValue(_)) => rv
          case _                                 => evalStatement(cur)
      }

private def evalStatement(stmt: Statement): Either[EvalError, Object] = stmt match
  case Statement.Expr(expr) => evalExpr(expr)
  case Statement.Return(expr) =>
    evalExpr(expr).map(_.get match
      case Some(v) => Object.ReturnValue(v)
      case _       => ConstNull
    )

  case _ => ???

private def evalExpr(expr: Expr): Either[EvalError, Object] = expr match
  case Expr.Int(Token.Int(v)) => Right(Object.Int(v))
  case Expr.Bool(t)           => Right(Object.Boolean(t.equals(Token.True)))
  case expr: Expr.Prefix      => evalPrefixExpr(expr)
  case expr: Expr.Infix       => evalInfixExpr(expr)
  case expr: Expr.If          => evalIfExpr(expr)
  case _                      => ???

private def evalPrefixExpr(item: Expr.Prefix): Either[EvalError, Object] =
  val Expr.Prefix(t, expr) = item
  lazy val r = evalExpr(expr)

  r.map { right =>
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
      case _ => ???
  }

private def evalInfixExpr(item: Expr.Infix): Either[EvalError, Object] = for {
  expOfL <- evalExpr(item.left)
  expOfR <- evalExpr(item.right)
} yield item.token match
  case t: (Token.Plus.type | Token.Asterisk.type) =>
    (expOfL, expOfR) match
      case (Object.Int(l), Object.Int(r)) =>
        val result = t match
          case Token.Plus     => l + r
          case Token.Asterisk => l * r
        Object.Int(result)
      case (Object.Boolean(l), Object.Boolean(r)) =>
        Object.Boolean {
          t match
            case Token.Plus     => l || r
            case Token.Asterisk => l && r
        }
      case _ => ConstNull

  case t: (Token.Minus.type | Token.Slash.type) =>
    (expOfL, expOfR) match
      case (Object.Int(l), Object.Int(r)) =>
        val result = t match
          case Token.Minus => l - r
          case Token.Slash => l / r
        Object.Int(result)
      case _ => ConstNull

  case t: (Token.Lt.type | Token.Gt.type) =>
    (expOfL, expOfR) match
      case (Object.Int(l), Object.Int(r)) =>
        Object.Boolean {
          t match
            case Token.Lt => l < r
            case Token.Gt => l > r
        }
      case _ => ConstNull

  case t: (Token.Eq.type | Token.NotEq.type) =>
    val result = for {
      l <- expOfL.get
      r <- expOfR.get
      value = t match
        case Token.Eq    => l == r
        case Token.NotEq => l != r
    } yield Object.Boolean(value)
    result getOrElse ConstNull

  case Token.LeftParen => ???

private def evalIfExpr(item: Expr.If): Either[EvalError, Object] =
  lazy val consequence = evalProgram(item.consequence)
  lazy val alter =
    item.alter match
      case Some(alter) => evalProgram(alter)
      case None        => Right(ConstNull)

  evalExpr(item.cond).flatMap {
    case Object.Boolean(bool)      => if bool then consequence else alter
    case Object.Int(value)         => consequence
    case Object.ReturnValue(value) => ???
    case Object.Null               => alter
  }

private val ConstNull = obj.Object.Null
