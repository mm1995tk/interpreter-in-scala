package evaluator

import ast.*
import obj.*
import ast.{Statement, Expr}
import token.{Token, InfixToken, PrefixToken}
import parser.{ParserError}
import token.showLiteral
import env.Env
import cats.implicits._
import cats.data.StateT
import cats.Show

type EitherEvalErrorOr[T] = Either[EvalError, T]

type Evaluator[T] = StateT[EitherEvalErrorOr, Env, T]

def evalProgram(program: Program): Evaluator[Object] =
  program.headOption match
    case None => StateT.pure(ConstNull)
    case Some(h) =>
      program.tail.foldLeft(evalStatement(h)) { (acc, cur) =>
        for {
          obj <- acc
          result <- obj match
            case Object.ReturnValue(obj) => StateT.pure[EitherEvalErrorOr, Env, Object](obj)
            case _                       => evalStatement(cur)
        } yield result
      }

private def evalStatement(stmt: Statement): Evaluator[Object] = stmt match
  case Statement.Expr(expr) => evalExpr(expr)
  case Statement.Return(expr) =>
    evalExpr(expr).map {
      case obj: MonkeyPrimitiveType    => Object.ReturnValue(obj)
      case obj @ Object.ReturnValue(_) => obj
    }

  case Statement.Let(ident, expr) =>
    for {
      monkeyPrimitiveType: MonkeyPrimitiveType <- evalExpr(expr).map(_.unwrap)
      env <- Utils.getEnv
      updatedEnv: Env = env.updated(ident.value, monkeyPrimitiveType)
      _ <- Utils.setEnv(updatedEnv)
    } yield ConstNull

private def evalExpr(expr: Expr): Evaluator[Object] = expr match
  case Expr.Int(Token.Int(v)) => Utils.liftEvaluator[Object](Right(Object.Int(v)))
  case Expr.Bool(t) =>
    StateT.pure(Object.Boolean { t.equals(Token.True) })
  case expr: Expr.Prefix =>
    evalPrefixExpr(expr).map((item: Object) => item)
  case expr: Expr.Infix    => evalInfixExpr(expr).map((item: Object) => item)
  case expr: Expr.If       => evalIfExpr(expr).map((item: Object) => item)
  case Expr.Null           => StateT.pure(ConstNull)
  case Expr.Call(fn, args) => evalCallExpr(fn, args)
  case Expr.Ident(t @ Token.Ident(key)) =>
    for {
      eitherObjOrErr <- Utils.getEnv.map {
        _.get(key) match
          case Some(obj: MonkeyPrimitiveType) => Right(obj)
          case Some(Object.ReturnValue(obj))  => Right(obj)
          case None                           => Left(EvalError.UncaughtReferenceError(t))
      }
      obj <- StateT.lift(eitherObjOrErr)
    } yield obj

  case Expr.Fn(params, body) =>
    Utils.getEnv.map { Object.Function(params.map(_.token), body, _) }

private def evalCallExpr(
    fn: Expr.Ident | Expr.Fn,
    args: Seq[Expr]
): Evaluator[Object] = for {
  env <- Utils.getEnv
  fnObj <- {
    fn match
      case Expr.Fn(params, body) => StateT.pure(Object.Function(params.map(_.token), body, env))

      case Expr.Ident(t @ Token.Ident(key)) =>
        StateT.lift {
          env.get(key).toRight(EvalError.UncaughtReferenceError(t)).flatMap {
            case obj: Object.Function => Right(obj)
            case _                    => Left(???) // 関数以外のエラー
          }
        }
  }: Evaluator[Object.Function]

  evaluatedArgs: Seq[(String, MonkeyPrimitiveType)] <-
    if fnObj.params.length.equals(args.length) then
      args
        .map(evalExpr(_).map(_.unwrap))
        .sequence
        .map(_.zip(fnObj.params.map(_.value)).map(item => (item._2, item._1.unwrap)))
    else
      Utils.liftEvaluator[Seq[(String, MonkeyPrimitiveType)]] {
        Left(EvalError.CountOfArgsMismatch(args.length, fnObj.params.length))
      }

  localEnv = evaluatedArgs.foldLeft(fnObj.env) { (acc, cur) =>
    acc.updated(cur._1, cur._2)
  }

  result <- Utils.setEnv(env.concat(localEnv)) *> evalProgram(fnObj.program) <* Utils.setEnv(env)
} yield result

private def evalPrefixExpr(item: Expr.Prefix): Evaluator[MonkeyPrimitiveType] =
  val Expr.Prefix(t: PrefixToken, expr) = item

  evalExpr(expr).map {
    case Object.Int(v) =>
      t match
        case Token.Minus => Object.Int(-v)
        case Token.Bang  => Object.Boolean(false)
    case Object.Boolean(b) =>
      t match
        case Token.Minus => ConstNull
        case Token.Bang  => Object.Boolean(!b)
    case Object.Null => Object.Boolean(true)
    case obj: Object.Function =>
      return StateT.lift(Left(EvalError.UnknownOperator(t, obj: MonkeyPrimitiveType)))
    case Object.ReturnValue(value) => value
  }

private def evalInfixExpr(item: Expr.Infix): Evaluator[MonkeyPrimitiveType] = for {
  expOfL: MonkeyPrimitiveType <- evalExpr(item.left).map(_.unwrap)
  expOfR: MonkeyPrimitiveType <- evalExpr(item.right).map(_.unwrap)
  result <- (item.token match
    case Token.LeftParen => ???
    case t: (Token.Eq.type | Token.NotEq.type) =>
      StateT.pure(Object.Boolean {
        t match
          case Token.Eq    => expOfL == expOfR
          case Token.NotEq => expOfL != expOfR
      })

    case t: (Token.Plus.type | Token.Asterisk.type) => evalPlusOrMulOpInfixExpr(t, expOfL, expOfR)
    case t: (Token.Minus.type | Token.Slash.type)   => evalMinusOrModOpInfixExpr(t, expOfL, expOfR)
    case t: (Token.Lt.type | Token.Gt.type)         => evalCompareOpInfixExpr(t, expOfL, expOfR)
  ): Evaluator[MonkeyPrimitiveType]
} yield result

private def evalPlusOrMulOpInfixExpr(
    t: (Token.Plus.type | Token.Asterisk.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: Either[EvalError, MonkeyPrimitiveType] = (a, b) match
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

  StateT.lift(either)

private def evalMinusOrModOpInfixExpr(
    t: (Token.Minus.type | Token.Slash.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: Either[EvalError, MonkeyPrimitiveType] = (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Int {
        t match
          case Token.Minus => l - r
          case Token.Slash => l / r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))
  StateT.lift(either)

private def evalCompareOpInfixExpr(
    t: (Token.Lt.type | Token.Gt.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: Either[EvalError, MonkeyPrimitiveType] = (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Lt => l < r
          case Token.Gt => l > r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))
  StateT.lift(either)

private def evalIfExpr(item: Expr.If): Evaluator[Object] =

  lazy val consequence: Evaluator[Object] = evalProgram(item.consequence)
  lazy val alter: Evaluator[Object] =
    item.alter match
      case Some(alter) => evalProgram(alter)
      case None        => Utils.liftEvaluator[Object](Right(ConstNull))

  evalExpr(item.cond).flatMap {
    case Object.Boolean(bool)      => if bool then consequence else alter
    case Object.ReturnValue(value) => StateT.pure(value)
    case Object.Null               => alter
    case Object.Int(value)         => consequence
    case _: Object.Function        => consequence
  }

private val ConstNull: MonkeyPrimitiveType = Object.Null

private object Utils:
  def liftEvaluator[T] = StateT.lift[EitherEvalErrorOr, Env, T]
  def getEnv = StateT.get[EitherEvalErrorOr, Env]
  def setEnv = StateT.set[EitherEvalErrorOr, Env]

enum EvalError:
  case TypeMismatch(left: MonkeyPrimitiveType, right: MonkeyPrimitiveType, op: InfixToken)
  case UncaughtReferenceError(ident: Token.Ident)
  case UnknownOperator(op: PrefixToken, value: MonkeyPrimitiveType)
  case CountOfArgsMismatch(obtained: Int, expected: Int)

given Show[EvalError] with
  def show(t: EvalError): String = t match
    case EvalError.TypeMismatch(l, r, op) =>
      s"typemismatch: can't calculate ${l.getType} and ${r.getType} by ${op.showLiteral}."
    case EvalError.UncaughtReferenceError(Token.Ident(key)) =>
      s"uncaught referenceError: $key is not defined"
    case EvalError.UnknownOperator(op, value) => s"unknown operator: ${op.showLiteral}${value.getType}"
    case EvalError.CountOfArgsMismatch(obtained, expected) =>
      s"expected count of args is $expected, but got $obtained"
