package obj

import token.{InfixToken}

enum Object:
  def get: Option[MonkeyPrimitiveType] = this match
    case Int(value)         => Some(value)
    case Boolean(value)     => Some(value)
    case ReturnValue(value) => Some(value)
    case Null               => None

  def getType: String = this match
    case Int(_)         => "Int"
    case Boolean(_)     => "Boolean"
    case ReturnValue(v) => if v.isInstanceOf[Int] then "Int" else "Boolean"
    case Null           => "null"

  case Int(value: scala.Int)
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: MonkeyPrimitiveType)
  case Null

type MonkeyPrimitiveType = scala.Int | scala.Boolean
