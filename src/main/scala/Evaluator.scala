package evaluator

import ast.*
import lexer.*
import obj.*
import ast.{Statement, Expr}
import token.{Token, InfixToken, PrefixToken}
import parser.{ParserError}
import token.showLiteral
import env.Env
import cats.implicits._

def evalProgram(program: Program, env: Env): (Env, Either[EvalError, Object]) =
  program.headOption match
    case None => env -> Right(ConstNull)
    case Some(h) =>
      program.tail.foldLeft(evalStatement(h, env)) { (acc, cur) =>
        acc._2 match
          case err @ Left(_)                  => acc._1 -> err
          case Right(Object.ReturnValue(obj)) => acc._1 -> Right(obj)
          case _                              => evalStatement(cur, acc._1)
      }

private def evalStatement(stmt: Statement, env: Env): (Env, Either[EvalError, Object]) = stmt match
  case Statement.Expr(expr) => evalExpr(expr, env)
  case Statement.Return(expr) =>
    val (e, v) = evalExpr(expr, env)

    e -> v.map {
      case obj @ Object.ReturnValue(_) => obj
      case obj: MonkeyPrimitiveType    => Object.ReturnValue(obj)
    }

  case Statement.Let(ident, expr) =>
    val (e: Env, v) = evalExpr(expr, env)
    v match
      case err @ Left(_) => e -> err
      case rightObj @ Right(obj: MonkeyPrimitiveType) =>
        { e.updated(ident.value, obj): Env } -> rightObj

      case rightObj @ Right(Object.ReturnValue(obj)) =>
        { e.updated(ident.value, obj): Env } -> rightObj

private def evalExpr(expr: Expr, env: Env): (Env, Either[EvalError, Object]) = expr match
  case Expr.Int(Token.Int(v)) => env -> Right(Object.Int(v))
  case Expr.Bool(t)           => env -> Right(Object.Boolean(t.equals(Token.True)))
  case expr: Expr.Prefix      => evalPrefixExpr(expr, env)
  case expr: Expr.Infix       => evalInfixExpr(expr, env)
  case expr: Expr.If          => evalIfExpr(expr, env)
  case Expr.Null              => env -> Right(ConstNull)
  case Expr.Call(fn, args)    => env -> evalCallExpr(fn, args, env)
  case Expr.Ident(t @ Token.Ident(key)) =>
    env -> {
      env.get(key) match
        case Some(obj: MonkeyPrimitiveType) => Right(obj)
        case Some(Object.ReturnValue(obj))  => Right(obj)
        case None                           => Left(EvalError.UncaughtReferenceError(t))
    }
  case Expr.Fn(params, body) =>
    env -> Right { Object.Function(params.map(_.token), body, Env().concat(env)) }

private def evalCallExpr(
    fn: Expr.Ident | Expr.Fn,
    args: Seq[Expr],
    env: Env
): Either[EvalError, Object] = for {
  fnObj <- {
    fn match
      case Expr.Fn(params, body) =>
        // val env = params
        Right { Object.Function(params.map(_.token), body, Env().concat(env)) }
      case Expr.Ident(t @ Token.Ident(key)) =>
        env.get(key).toRight(EvalError.UncaughtReferenceError(t)).flatMap {
          case obj @ Object.Function(_, _, _) => Right(obj)
          case _                              => Left(???) // 関数以外のエラー
        }
  }: Either[EvalError, Object.Function]

  evaluatedArgs <- {
    if fnObj.params.length.equals(args.length) then
      val t: Either[EvalError, Seq[MonkeyPrimitiveType]] = {
        args
          .map(evalExpr(_, env)._2.flatMap {
            case Object.ReturnValue(_)      => Left(???)
            case other: MonkeyPrimitiveType => Right(other)
          }): Seq[Either[EvalError, MonkeyPrimitiveType]]
      }.sequence

      t.map(_.zip(fnObj.params.map(_.value)).map(item => (item._2, item._1)))
    else Left(EvalError.CountOfArgsMismatch(args.length, fnObj.params.length))
  }
  maybeHead = evaluatedArgs.headOption.map { item => env.updated(item._1, item._2): Env }
  localEnv = evaluatedArgs.tail.foldLeft(maybeHead.getOrElse(env)) { (acc, cur) =>
    acc.updated(cur._1, cur._2)
  }
  result <- evalProgram(fnObj.program, localEnv)._2
} yield result

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
    case Object.Null => Object.Boolean(true)
    case obj @ Object.Function(_, _, _) =>
      return e -> Left(EvalError.UnknownOperator(t, obj: MonkeyPrimitiveType))
    case Object.ReturnValue(value) => value
  }

private def evalInfixExpr(item: Expr.Infix, env: Env): (Env, Either[EvalError, MonkeyPrimitiveType]) =
  val (e1, l) = evalExpr(item.left, env)
  val (e2, r) = evalExpr(item.right, e1)

  val obj: Either[EvalError, MonkeyPrimitiveType] = for {
    expOfL <- l.map(_.unwrap): Either[EvalError, MonkeyPrimitiveType];
    expOfR <- r.map(_.unwrap): Either[EvalError, MonkeyPrimitiveType]
    result <- (item.token match
      case Token.LeftParen => ???
      case t: (Token.Eq.type | Token.NotEq.type) =>
        Right(Object.Boolean {
          t match
            case Token.Eq    => expOfL == expOfR
            case Token.NotEq => expOfL != expOfR
        })
      case t: (Token.Plus.type | Token.Asterisk.type) => evalPlusOrMulOpInfixExpr(t, expOfL, expOfR)
      case t: (Token.Minus.type | Token.Slash.type)   => evalMinusOrModOpInfixExpr(t, expOfL, expOfR)
      case t: (Token.Lt.type | Token.Gt.type)         => evalCompareOpInfixExpr(t, expOfL, expOfR)
    ): Either[EvalError, MonkeyPrimitiveType]
  } yield result

  e2 -> obj

private def evalPlusOrMulOpInfixExpr(
    t: (Token.Plus.type | Token.Asterisk.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Int {
        t match
          case Token.Plus     => l + r
          case Token.Asterisk => l * r
      })

    case (Object.Boolean(l), Object.Boolean(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Plus     => l || r
          case Token.Asterisk => l && r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

private def evalMinusOrModOpInfixExpr(
    t: (Token.Minus.type | Token.Slash.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Int {
        t match
          case Token.Minus => l - r
          case Token.Slash => l / r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

private def evalCompareOpInfixExpr(
    t: (Token.Lt.type | Token.Gt.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Either[EvalError, MonkeyPrimitiveType] =
  (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Lt => l < r
          case Token.Gt => l > r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

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
          case Object.Boolean(bool)      => if bool then consequence else alter
          case Object.Int(value)         => consequence
          case Object.ReturnValue(value) => Right(value)
          case Object.Null               => alter
          case Object.Function(_, _, _)  => consequence
      }

private val ConstNull: MonkeyPrimitiveType = Object.Null

enum EvalError:
  def show: String = this match
    case TypeMismatch(l, r, op) =>
      s"typemismatch: can't calculate ${l.getType} and ${r.getType} by ${op.showLiteral}."
    case UncaughtReferenceError(Token.Ident(key)) => s"uncaught referenceError: $key is not defined"
    case UnknownOperator(op, value)               => s"unknown operator: ${op.showLiteral}${value.getType}"
    case CountOfArgsMismatch(obtained, expected) => s"expected count of args is $expected, but got $obtained"
  case TypeMismatch(left: MonkeyPrimitiveType, right: MonkeyPrimitiveType, op: InfixToken)
  case UncaughtReferenceError(ident: Token.Ident)
  case UnknownOperator(op: PrefixToken, value: MonkeyPrimitiveType)
  case CountOfArgsMismatch(obtained: Int, expected: Int)
