package builtin

import obj.{Object, MonkeyPrimitiveType}
import env.Env
import scala.collection.immutable.HashMap

enum Builtin:
  case Len(f: Object.Str | Object.Arr => Object.Int)

object Builtin:
  def getEnv: Env =
    new HashMap[String, MonkeyPrimitiveType]()
      .updated(
        "len",
        Object.BuiltinObj(Builtin.Len {
          case Object.Arr(elems) => Object.Int(elems.length)
          case Object.Str(str)   => Object.Int(str.length)
        })
      )
