package env

import obj.{MonkeyPrimitiveType, Object}
import scala.collection.immutable.HashMap
import builtin.Builtin

type Env = HashMap[String, MonkeyPrimitiveType]

object Env:
  def apply(): Env = Builtin.getEnv
