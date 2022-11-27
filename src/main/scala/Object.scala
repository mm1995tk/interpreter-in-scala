package obj

import token.{InfixToken, Token}
import ast.{Statement, Program}
import env.Env
import cats.Show
import cats.implicits.toShow

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

  def getValue: Option[Int | Boolean | String] = item match
    case Object.Int(v)            => Some(v)
    case Object.Boolean(v)        => Some(v)
    case Object.Function(_, _, _) => Some(item.asInstanceOf[Object].show)
    case Object.Null              => None

given Show[Object] with
  def show(obj: Object) = obj match
    case Object.Int(value)         => value.toString()
    case Object.Boolean(value)     => value.toString()
    case Object.ReturnValue(value) => value.getValue.getOrElse("null").toString()
    case Object.Function(_, _, _)  => "function"
    case Object.Null               => "null"
