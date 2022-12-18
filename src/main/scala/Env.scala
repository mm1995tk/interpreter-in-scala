package env

import obj.{MonkeyPrimitiveType, Object}
import scala.collection.immutable.HashMap
import evaluator.builtin.Builtin

type Env = HashMap[String, MonkeyPrimitiveType]

object Env:
  def apply(): Env = Builtin.getEnv
