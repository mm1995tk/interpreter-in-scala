package obj

import token.{InfixToken}

enum Object:
  case Int(value: scala.Int)
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: MonkeyPrimitiveType)
  case Null

type MonkeyPrimitiveType = Object.Int | Object.Boolean | Object.Null.type

extension (item: MonkeyPrimitiveType)
  def getType: String = item match
    case Object.Int(_)     => "Int"
    case Object.Boolean(_) => "Boolean"
    case Object.Null       => "null"

  def getValue = item match
    case Object.Int(v)     => Some(v)
    case Object.Boolean(v) => Some(v)
    case Object.Null       => None
