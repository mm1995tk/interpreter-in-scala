package evaluator.builtin

import obj.{Object, MonkeyPrimitiveType}
import evaluator.{Evaluator, EvalError}

case class Builtin(f: Seq[Object] => Evaluator[Object], cntOfArgs: scala.Int)

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

def push: Builtin = Builtin(
  {
    case h1 :: h2 :: Seq() =>
      h1 match
        case Object.Arr(elems) => Evaluator.pure(Object.Arr(elems :+ h2))
        case other             => Evaluator.pureErr(???)
    case seq => Evaluator.pureErr(EvalError.CountOfArgsMismatch(seq.length, 2))
  },
  2
)
