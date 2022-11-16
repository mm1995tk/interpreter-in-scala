package env

import obj.MonkeyPrimitiveType
import scala.collection.immutable.HashMap

type Env = HashMap[String, MonkeyPrimitiveType]

object Env:
  def apply(): Env = new HashMap[String, MonkeyPrimitiveType]()
