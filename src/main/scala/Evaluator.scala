package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.Token
import parser.{ParserError, ParserErrors}
import java.util.Objects

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
  case expr: Expr.Infix       => evalInfixExpr(expr)
  case expr: Expr.If          => evalIfExpr(expr)
  case _                      => ???

private def evalPrefixExpr(item: Expr.Prefix) =
  val Expr.Prefix(t, expr) = item
  lazy val right = evalExpr(expr)
  
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

private def evalInfixExpr(item: Expr.Infix): Object =
  val expOfL = evalExpr(item.left)
  val expOfR = evalExpr(item.right)
  item.token match
    case t: (Token.Plus.type | Token.Asterisk.type) =>
      (expOfL, expOfR) match
        case (Object.Int(l), Object.Int(r)) =>
          val result = t match
            case Token.Plus     => l + r
            case Token.Asterisk => l * r
          Object.Int(result)
        case (Object.Boolean(l), Object.Boolean(r)) =>
          val result = t match
            case Token.Plus     => l || r
            case Token.Asterisk => l && r
          Object.Boolean(l || r)
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

private def evalIfExpr(item: Expr.If): Object =

  lazy val consequence = Evaluator(Right(item.consequence)).getOrElse(ConstNull)
  lazy val alter =
    item.alter match
      case Some(alter) => Evaluator(Right(alter)).getOrElse(ConstNull)
      case None        => ConstNull

  evalExpr(item.cond) match
    case Object.Boolean(bool) => if bool then consequence else alter
    case Object.Int(value)    => consequence
    case Object.Null          => alter

private val ConstNull = obj.Object.Null
