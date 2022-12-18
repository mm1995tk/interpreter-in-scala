package env

import obj.{MonkeyPrimitiveType, Object}
import scala.collection.immutable.HashMap
import evaluator.builtin.*

type Env = HashMap[String, MonkeyPrimitiveType]

def Env(): Env = HashMap[String, MonkeyPrimitiveType](
  "len" -> Object.BuiltinObj(len),
  "head" -> Object.BuiltinObj(head),
  "tail" -> Object.BuiltinObj(tail)
)
