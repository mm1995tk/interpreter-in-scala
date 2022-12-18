package evaluator.builtin

import obj.{Object, MonkeyPrimitiveType}
import env.Env
import scala.collection.immutable.HashMap
import evaluator.{Evaluator, EvalError}

case class Builtin(f: Seq[Object] => Evaluator[Object], cntOfArgs: scala.Int)

object Builtin:
  def getEnv: Env =
    new HashMap[String, MonkeyPrimitiveType]()
      .updated(
        "len",
        Object.BuiltinObj(len)
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
