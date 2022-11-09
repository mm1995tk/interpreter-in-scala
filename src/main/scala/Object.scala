package obj

import token.{InfixToken}

enum Object:
  def get: Option[Type] = this match
    case Int(value)         => Some(value)
    case Boolean(value)     => Some(value)
    case ReturnValue(value) => Some(value)
    case Null               => None

  case Int(value: scala.Int)
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: Type)
  case Null

type Type = scala.Int | scala.Boolean

enum EvalError:
  case TypeMismatch(left: Object, right: Object, op: InfixToken)
