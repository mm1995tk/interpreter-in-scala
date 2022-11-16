package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.{Token, InfixToken, PrefixToken}
import parser.{ParserError}
import token.showLiteral
import env.Env

def evalProgram(program: Program, env: Env): (Env, Either[EvalError, Object]) =
  program.headOption match
    case None => env -> Right(ConstNull)
    case Some(h) =>
      program.tail.foldLeft(evalStatement(h, env)) { (acc, cur) =>
        acc._2 match
          case err @ Left(_)                 => acc._1 -> err
          case Right(Object.ReturnValue(rv)) => acc._1 -> Right(rv)
          case _                             => evalStatement(cur, acc._1)
      }

private def evalStatement(stmt: Statement, env: Env): (Env, Either[EvalError, Object]) = stmt match
  case Statement.Expr(expr) => evalExpr(expr, env)
  case Statement.Return(expr) =>
    val (e, v) = evalExpr(expr, env)

    e -> v.map {
      case a @ Object.ReturnValue(v) => a
      case b: MonkeyPrimitiveType    => Object.ReturnValue(b)
    }

  case Statement.Let(ident, expr) =>
    val (e: Env, v) = evalExpr(expr, env)
    v match
      case rightObj @ Right(obj: MonkeyPrimitiveType) =>
        val g: Either[EvalError, Object] = rightObj
        val k: Env = e.updated(ident.value, obj)
        k -> g
      case err @ Left(_)                    => e -> err
      case Right(Object.ReturnValue(value)) => e -> Right(value)

private def evalExpr(expr: Expr, env: Env): (Env, Either[EvalError, Object]) = expr match
  case Expr.Int(Token.Int(v)) => env -> Right(Object.Int(v))
  case Expr.Bool(t)           => env -> Right(Object.Boolean(t.equals(Token.True)))
  case expr: Expr.Prefix      => evalPrefixExpr(expr, env)
  case expr: Expr.Infix       => evalInfixExpr(expr, env)
  case expr: Expr.If          => evalIfExpr(expr, env)
  case Expr.Null              => env -> Right(ConstNull)
  case Expr.Ident(t @ Token.Ident(key)) =>
    env -> {
      env.get(key) match
        case Some(obj: MonkeyPrimitiveType) => Right(obj)
        case Some(Object.ReturnValue(obj))  => Right(obj)
        case None                           => Left(EvalError.UncaughtReferenceError(t))
    }

  case _ => ???

private def evalPrefixExpr(item: Expr.Prefix, env: Env): (Env, Either[EvalError, MonkeyPrimitiveType]) =
  val Expr.Prefix(t: PrefixToken, expr) = item

  val (e, v) = evalExpr(expr, env)

  e -> v.map {
    case Object.Int(v) =>
      t match
        case Token.Minus => Object.Int(-v)
        case Token.Bang  => Object.Boolean(false)
    case Object.Boolean(b) =>
      t match
        case Token.Minus => ConstNull
        case Token.Bang  => Object.Boolean(!b)
    case Object.Null               => Object.Boolean(true)
    case Object.ReturnValue(value) => value
  }

def poi(
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType,
    t: (Token.Plus.type | Token.Asterisk.type)
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      val result = t match
        case Token.Plus     => l + r
        case Token.Asterisk => l * r
      Right(Object.Int(result))
    case (Object.Boolean(l), Object.Boolean(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Plus     => l || r
          case Token.Asterisk => l && r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

def poi2(
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType,
    t: (Token.Minus.type | Token.Slash.type)
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      val result = t match
        case Token.Minus => l - r
        case Token.Slash => l / r
      Right(Object.Int(result))
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

def poi3(
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType,
    t: (Token.Lt.type | Token.Gt.type)
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Lt => l < r
          case Token.Gt => l > r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

private def evalInfixExpr(item: Expr.Infix, env: Env): (Env, Either[EvalError, MonkeyPrimitiveType]) =
  val (e1, l) = evalExpr(item.left, env)
  val (e2, r) = evalExpr(item.right, e1)
  e2 -> {
    for {
      expOfL <- l
      expOfR <- r
    } yield item.token match

      case t: (Token.Plus.type | Token.Asterisk.type) =>
        {
          (expOfL, expOfR) match
            case (Object.ReturnValue(obj1), Object.ReturnValue(obj2)) =>
              poi(obj1, obj2, t)
            case (Object.ReturnValue(obj1), obj2: MonkeyPrimitiveType) =>
              poi(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, Object.ReturnValue(obj2)) =>
              poi(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, obj2: MonkeyPrimitiveType) =>
              poi(obj1, obj2, t)
        } match
          case e @ Left(_)  => return e2 -> e
          case Right(value) => value

      case t: (Token.Minus.type | Token.Slash.type) =>
        {
          (expOfL, expOfR) match
            case (Object.ReturnValue(obj1), Object.ReturnValue(obj2)) =>
              poi2(obj1, obj2, t)
            case (Object.ReturnValue(obj1), obj2: MonkeyPrimitiveType) =>
              poi2(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, Object.ReturnValue(obj2)) =>
              poi2(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, obj2: MonkeyPrimitiveType) =>
              poi2(obj1, obj2, t)
        } match
          case e @ Left(_)  => return e2 -> e
          case Right(value) => value
      // (expOfL, expOfR) match
      //   case (Object.Int(l), Object.Int(r)) =>
      //     val result = t match
      //       case Token.Minus => l - r
      //       case Token.Slash => l / r
      //     Object.Int(result)
      //   case (l, r) => return e2 -> Left(EvalError.TypeMismatch(l, r, t))

      case t: (Token.Lt.type | Token.Gt.type) =>
        {
          (expOfL, expOfR) match
            case (Object.ReturnValue(obj1), Object.ReturnValue(obj2)) =>
              poi3(obj1, obj2, t)
            case (Object.ReturnValue(obj1), obj2: MonkeyPrimitiveType) =>
              poi3(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, Object.ReturnValue(obj2)) =>
              poi3(obj1, obj2, t)
            case (obj1: MonkeyPrimitiveType, obj2: MonkeyPrimitiveType) =>
              poi3(obj1, obj2, t)
        } match
          case e @ Left(_)  => return e2 -> e
          case Right(value) => value
      // (expOfL, expOfR) match
      //   case (Object.Int(l), Object.Int(r)) =>
      //     Object.Boolean {
      //       t match
      //         case Token.Lt => l < r
      //         case Token.Gt => l > r
      //     }
      //   case (l, r) => return e2 -> Left(EvalError.TypeMismatch(l, r, t))

      case t: (Token.Eq.type | Token.NotEq.type) =>
        val result: Option[MonkeyPrimitiveType] = for {
          l <- expOfL match
            case Object.ReturnValue(obj: MonkeyPrimitiveType) => obj.getValue
            case obj: MonkeyPrimitiveType                     => obj.getValue

          r <- expOfR match
            case Object.ReturnValue(obj: MonkeyPrimitiveType) => obj.getValue
            case obj: MonkeyPrimitiveType                     => obj.getValue
          value = t match
            case Token.Eq    => l == r
            case Token.NotEq => l != r
        } yield Object.Boolean(value)
        result getOrElse ConstNull

      case Token.LeftParen => ???
  }

private def evalIfExpr(item: Expr.If, env: Env): (Env, Either[EvalError, Object]) =
  val (e, v) = evalExpr(item.cond, env)
  val localEnv: Env = Env().concat(e)

  lazy val (_, consequence) = evalProgram(item.consequence, localEnv)

  lazy val (_, alter) =
    item.alter match
      case Some(alter) => evalProgram(alter, localEnv)
      case None        => e -> Right(ConstNull)

  v match
    case err @ Left(_) => e -> err
    case Right(obj) =>
      e -> {
        obj match
          case Object.Boolean(bool) =>
            if bool then consequence
            else alter
          case Object.Int(value) =>
            consequence

          case Object.ReturnValue(value) => Right(value)

          case Object.Null =>
            alter
      }

private val ConstNull: MonkeyPrimitiveType = Object.Null

enum EvalError:
  def show: String = this match
    case TypeMismatch(l, r, op) =>
      s"typemismatch: can't calculate ${l.getType} and ${r.getType} by ${op.showLiteral}."
    case UncaughtReferenceError(Token.Ident(key)) => s"uncaught referenceError: $key is not defined"

  case TypeMismatch(left: MonkeyPrimitiveType, right: MonkeyPrimitiveType, op: InfixToken)
  case UncaughtReferenceError(ident: Token.Ident)
