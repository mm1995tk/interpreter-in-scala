package evaluator

import ast.{*, given}
import obj.{*, given}
import ast.{Statement, Expr}
import token.{Token, InfixToken, PrefixToken}
import parser.{ParserError}
import token.given
import env.Env
import cats.implicits._
import cats.data.StateT
import cats.Show
import builtin.Builtin

type Evaluator[T] = StateT[EitherEvalErrorOr, Env, T]
type EitherEvalErrorOr[T] = Either[EvalError, T]

def evalProgram(program: Program): Evaluator[Object] = program match
  case Seq() => Evaluator.pure(ConstNull)
  case h :: tail =>
    tail.foldLeft(evalStatement(h)) { (acc, cur) =>
      acc.flatMap {
        case obj: Object.ReturnValue => Evaluator.pure(obj)
        case _                       => evalStatement(cur)
      }
    }

private def evalStatement(stmt: Statement): Evaluator[Object] = stmt match
  case Statement.Expr(expr, isSemicolon) =>
    val stmtExpr = evalExpr(expr)
    if isSemicolon then
      stmtExpr.map {
        case obj: Object.ReturnValue => obj
        case other                   => ConstNull
      }
    else stmtExpr

  case Statement.Return(expr) =>
    evalExpr(expr).map {
      case obj: MonkeyPrimitiveType => Object.ReturnValue(obj)
      case obj: Object.ReturnValue  => obj
    }

  case Statement.Let(ident, expr) =>
    for {
      monkeyPrimitiveType: MonkeyPrimitiveType <- evalExpr(expr).map(_.unwrap)
      env <- Evaluator.getEnv
      updatedEnv: Env = env.updated(ident.value, monkeyPrimitiveType)
      _ <- Evaluator.setEnv(updatedEnv)
    } yield ConstNull

private def evalExpr(expr: Expr): Evaluator[Object] = expr match
  case Expr.Int(Token.Int(v)) => Evaluator.pure(Object.Int(v))
  case Expr.Str(Token.Str(v)) => Evaluator.pure(Object.Str(v))
  case Expr.Bool(t) =>
    Evaluator.pure(Object.Boolean { t.equals(Token.True) })
  case expr: Expr.Prefix   => evalPrefixExpr(expr)
  case expr: Expr.Infix    => evalInfixExpr(expr)
  case expr: Expr.If       => evalIfExpr(expr)
  case Expr.Null           => Evaluator.pure(ConstNull)
  case Expr.Call(fn, args) => evalCallExpr(fn, args)
  case Expr.Ident(t @ Token.Ident(key)) =>
    for {
      eitherObjOrErr <- Evaluator.getEnv.map {
        _.get(key) match
          case Some(obj: MonkeyPrimitiveType) => Right(obj)
          case Some(Object.ReturnValue(obj))  => Right(obj)
          case None                           => Left(EvalError.UncaughtReferenceError(t))
      }
      obj <- Evaluator.lift(eitherObjOrErr)
    } yield obj

  case Expr.Fn(params, body) =>
    Evaluator.getEnv.map { Object.Function(params.map(_.token), body, _) }

private def evalCallExpr(
    fn: Expr.Ident | Expr.Fn | Expr.Call | Expr.If,
    args: Seq[Expr]
): Evaluator[Object] = for {
  env <- Evaluator.getEnv
  fnObj <- {
    fn match
      case Expr.Fn(params, body) => Evaluator.pure(Object.Function(params.map(_.token), body, env))

      case expr: Expr.If =>
        evalIfExpr(expr).flatMap {
          case obj: (Object.Function | Object.BuiltinObj) => Evaluator.pure(obj)
          case _                                          => Evaluator.pureErr(???)
        }

      case Expr.Ident(t @ Token.Ident(key)) =>
        Evaluator.lift {
          env.get(key).toRight(EvalError.UncaughtReferenceError(t)).flatMap {
            case obj: (Object.Function | Object.BuiltinObj) => Right(obj)
            case _                                          => Left(???) // 関数以外のエラー
          }
        }

      case Expr.Call(fn, params) =>
        evalCallExpr(fn, params).flatMap {
          case obj: Object.Function                     => Evaluator.pure(obj)
          case Object.ReturnValue(obj: Object.Function) => Evaluator.pure(obj)
          case _                                        => Evaluator.pureErr(???)
        }

  }: Evaluator[Object.Function | Object.BuiltinObj]

  cntOfExpectedParams = fnObj match
    case obj: Object.Function => obj.params.length
    case Object.BuiltinObj(builtin) =>
      builtin match
        case Builtin.Len(f) => 1

  _ <-
    if args.length.equals(cntOfExpectedParams) then Evaluator.pure(())
    else
      Evaluator.pureErr {
        EvalError.CountOfArgsMismatch(args.length, cntOfExpectedParams)
      }

  result <- fnObj match
    case fnObj: Object.Function =>
      for {
        evaluatedArgs: Seq[(String, MonkeyPrimitiveType)] <- args
          .map(evalExpr(_).map(_.unwrap))
          .sequence
          .map(_.zip(fnObj.params.map(_.value)).map(item => (item._2, item._1.unwrap)))

        localEnv = evaluatedArgs.foldLeft(fnObj.env) { (acc, cur) =>
          acc.updated(cur._1, cur._2)
        }

        result <- Evaluator.setEnv(env.concat(localEnv)) *> evalProgram(fnObj.program) <* Evaluator.setEnv(
          env
        )
      } yield result
    case Object.BuiltinObj(builtin) =>
      builtin match
        case Builtin.Len(f) =>
          args.map(evalExpr(_).map(_.unwrap)).head.flatMap {
            case obj: Object.Str => Evaluator.pure(f(obj))
            case _               => Evaluator.pureErr(???)
          }

} yield result

private def evalPrefixExpr(item: Expr.Prefix): Evaluator[Object] =
  val Expr.Prefix(t: PrefixToken, expr) = item

  evalExpr(expr).map {
    case Object.Int(v) =>
      t match
        case Token.Minus => Object.Int(-v)
        case Token.Bang  => Object.Boolean(false)
    case obj @ Object.Str(v) =>
      t match
        case Token.Minus => return Evaluator.pureErr(EvalError.UnknownOperator(t, obj: MonkeyPrimitiveType))
        case Token.Bang  => Object.Boolean(false)
    case Object.Boolean(b) =>
      t match
        case Token.Minus => ConstNull
        case Token.Bang  => Object.Boolean(!b)
    case Object.Null => Object.Boolean(true)
    case obj: Object.Function =>
      return Evaluator.pureErr(EvalError.UnknownOperator(t, obj: MonkeyPrimitiveType))
    case Object.ReturnValue(value) => value
    case _                         => return Evaluator.pureErr(???)
  }

private def evalInfixExpr(item: Expr.Infix): Evaluator[Object] = for {
  expOfL: MonkeyPrimitiveType <- evalExpr(item.left).map(_.unwrap)
  expOfR: MonkeyPrimitiveType <- evalExpr(item.right).map(_.unwrap)
  result <- item.token match
    case t: (Token.Eq.type | Token.NotEq.type) =>
      Evaluator.pure(Object.Boolean {
        t match
          case Token.Eq    => expOfL == expOfR
          case Token.NotEq => expOfL != expOfR
      })

    case t: (Token.Plus.type | Token.Asterisk.type) => evalPlusOrMulOpInfixExpr(t, expOfL, expOfR)
    case t: (Token.Minus.type | Token.Slash.type)   => evalMinusOrModOpInfixExpr(t, expOfL, expOfR)
    case t: (Token.Lt.type | Token.Gt.type)         => evalCompareOpInfixExpr(t, expOfL, expOfR)

} yield result

private def evalPlusOrMulOpInfixExpr(
    t: (Token.Plus.type | Token.Asterisk.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: EitherEvalErrorOr[MonkeyPrimitiveType] = (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Int {
        t match
          case Token.Plus     => l + r
          case Token.Asterisk => l * r
      })

    case (lObj @ Object.Str(l), rObj @ Object.Str(r)) =>
      t match
        case Token.Plus     => Right(Object.Str(l + r))
        case Token.Asterisk => Left(EvalError.TypeMismatch(lObj, rObj, t))

    case (Object.Boolean(l), Object.Boolean(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Plus     => l || r
          case Token.Asterisk => l && r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))

  Evaluator.lift(either)

private def evalMinusOrModOpInfixExpr(
    t: (Token.Minus.type | Token.Slash.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: EitherEvalErrorOr[MonkeyPrimitiveType] = (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Int {
        t match
          case Token.Minus => l - r
          case Token.Slash => l / r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))
  Evaluator.lift(either)

private def evalCompareOpInfixExpr(
    t: (Token.Lt.type | Token.Gt.type),
    a: MonkeyPrimitiveType,
    b: MonkeyPrimitiveType
): Evaluator[MonkeyPrimitiveType] =
  val either: EitherEvalErrorOr[MonkeyPrimitiveType] = (a, b) match
    case (Object.Int(l), Object.Int(r)) =>
      Right(Object.Boolean {
        t match
          case Token.Lt => l < r
          case Token.Gt => l > r
      })
    case (l: MonkeyPrimitiveType, r: MonkeyPrimitiveType) => Left(EvalError.TypeMismatch(l, r, t))
  Evaluator.lift(either)

private def evalIfExpr(item: Expr.If): Evaluator[Object] =
  lazy val consequence: Evaluator[Object] = evalProgram(item.consequence)
  lazy val alter: Evaluator[Object] =
    item.alter match
      case Some(alter) => evalProgram(alter)
      case None        => Evaluator.pure(ConstNull)

  def go(item: Evaluator[Object]): Evaluator[Object] = item.flatMap {
    case Object.ReturnValue(value) => go(Evaluator.pure(value))
    case Object.Boolean(bool)      => if bool then consequence else alter
    case Object.Null               => alter
    case Object.Int(value)         => consequence
    case Object.Str(value)         => consequence
    case _                         => consequence
  }

  go(evalExpr(item.cond))

private val ConstNull: MonkeyPrimitiveType = Object.Null

private object Evaluator:
  def pure[T](item: T): Evaluator[T] = StateT.pure(item)
  def pureErr[T](item: EvalError): Evaluator[T] = StateT.lift(Left(item))
  def lift[T] = StateT.lift[EitherEvalErrorOr, Env, T]
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
      s"typemismatch: can't calculate ${l.getType} and ${r.getType} by ${op.asInstanceOf[Token].show}."
    case EvalError.UncaughtReferenceError(Token.Ident(key)) =>
      s"uncaught referenceError: $key is not defined"
    case EvalError.UnknownOperator(op, value) =>
      s"unknown operator: ${op.asInstanceOf[Token].show}${value.getType}"
    case EvalError.CountOfArgsMismatch(obtained, expected) =>
      s"expected count of args is $expected, but got $obtained"
