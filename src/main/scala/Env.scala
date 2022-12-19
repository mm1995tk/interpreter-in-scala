package env

import obj.{MonkeyPrimitiveType, Object}
import evaluator.builtin.*

type Env = Map[String, MonkeyPrimitiveType]

def Env(): Env = Map[String, MonkeyPrimitiveType](
  "len" -> Object.BuiltinObj(len),
  "head" -> Object.BuiltinObj(head),
  "tail" -> Object.BuiltinObj(tail),
  "push" -> Object.BuiltinObj(push)
)
