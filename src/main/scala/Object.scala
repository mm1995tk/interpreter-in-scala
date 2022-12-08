package obj

import token.{InfixToken, Token}
import ast.{Statement, Program}
import env.Env
import cats.Show
import cats.implicits.toShow
import builtin.Builtin

enum Object:
  def unwrap: MonkeyPrimitiveType = this match
    case Object.ReturnValue(obj: MonkeyPrimitiveType) => obj
    case obj: MonkeyPrimitiveType                     => obj

  case Int(value: scala.Int)
  case Str(value: String)
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: MonkeyPrimitiveType)
  case Function(params: Seq[Token.Ident], program: Program, env: Env)
  case BuiltinObj(builtin: Builtin)
  case Null

type MonkeyPrimitiveType = Object.Int | Object.Boolean | Object.Null.type | Object.Function | Object.Str |
  Object.BuiltinObj

extension (item: MonkeyPrimitiveType)
  def getType: String = item match
    case Object.Int(_)            => "Int"
    case Object.Str(_)            => "String"
    case Object.Boolean(_)        => "Boolean"
    case Object.Null              => "null"
    case Object.Function(_, _, _) => "function"
    case obj: Object.BuiltinObj   => "builtin function"

  def getValue: Option[Int | Boolean | String] = item match
    case Object.Int(v)            => Some(v)
    case Object.Str(v)            => Some(v)
    case Object.Boolean(v)        => Some(v)
    case Object.Function(_, _, _) => Some(item.asInstanceOf[Object].show)
    case Object.BuiltinObj(b)     => Some(item.asInstanceOf[Object].show)
    case Object.Null              => None

given Show[Object] with
  def show(obj: Object) = obj match
    case Object.Int(value)         => value.toString()
    case Object.Str(value)         => value
    case Object.Boolean(value)     => value.toString()
    case Object.ReturnValue(value) => value.getValue.getOrElse("null").toString()
    case Object.Function(_, _, _)  => "function"
    case Object.BuiltinObj(_)      => "builtin"
    case Object.Null               => "null"
