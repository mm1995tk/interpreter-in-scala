package obj

import token.{InfixToken, Token}
import ast.{Statement, Program}
import env.Env

enum Object:
  def unwrap: MonkeyPrimitiveType = this match
    case Object.ReturnValue(obj: MonkeyPrimitiveType) => obj
    case obj: MonkeyPrimitiveType                     => obj

  case Int(value: scala.Int)
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: MonkeyPrimitiveType)
  case Function(params: Seq[Token.Ident], program: Program, env: Env)
  case Null

type MonkeyPrimitiveType = Object.Int | Object.Boolean | Object.Null.type | Object.Function

extension (item: MonkeyPrimitiveType)
  def getType: String = item match
    case Object.Int(_)            => "Int"
    case Object.Boolean(_)        => "Boolean"
    case Object.Null              => "null"
    case Object.Function(_, _, _) => "function"

  def getValue = item match
    case Object.Int(v)            => Some(v)
    case Object.Boolean(v)        => Some(v)
    case Object.Function(_, _, _) => Some("function")
    case Object.Null              => None
