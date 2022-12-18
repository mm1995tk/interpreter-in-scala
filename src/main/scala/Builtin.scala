package evaluator.builtin

import obj.{Object, MonkeyPrimitiveType}
import env.Env
import scala.collection.immutable.HashMap
import evaluator.{Evaluator, EvalError}

case class Builtin(f: Seq[Object] => Evaluator[Object], cntOfArgs: scala.Int)

object Builtin:
  def getEnv: Env =
    HashMap[String, MonkeyPrimitiveType](
      "len" -> Object.BuiltinObj(len),
      "head" -> Object.BuiltinObj(head),
      "tail" -> Object.BuiltinObj(tail)
    )

def len: Builtin = Builtin(
  {
    case h :: Seq() =>
      h match
        case Object.Arr(elems) => Evaluator.pure(Object.Int(elems.length))
        case Object.Str(str)   => Evaluator.pure(Object.Int(str.length))
        case other             => Evaluator.pureErr(???)
    case seq => Evaluator.pureErr(EvalError.CountOfArgsMismatch(seq.length, 1))
  },
  1
)

def head: Builtin = Builtin(
  {
    case h :: Seq() =>
      h match
        case Object.Arr(elems) => Evaluator.pure(elems.headOption.getOrElse(Object.Null))
        case other             => Evaluator.pureErr(???)
    case seq => Evaluator.pureErr(EvalError.CountOfArgsMismatch(seq.length, 1))
  },
  1
)

def tail: Builtin = Builtin(
  {
    case h :: Seq() =>
      h match
        case Object.Arr(elems) => Evaluator.pure(Object.Arr(elems.tail))
        case other             => Evaluator.pureErr(???)
    case seq => Evaluator.pureErr(EvalError.CountOfArgsMismatch(seq.length, 1))
  },
  1
)
