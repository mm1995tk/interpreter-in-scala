package builtin

import obj.{Object, MonkeyPrimitiveType}
import env.Env
import scala.collection.immutable.HashMap

enum Builtin:
  case Len(f: Object.Str => Object.Int)

object Builtin:
  def getEnv: Env =
    new HashMap[String, MonkeyPrimitiveType]()
      .updated("len", Object.BuiltinObj(Builtin.Len(strObj => Object.Int(strObj.value.length))))
