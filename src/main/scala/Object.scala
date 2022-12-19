package obj

import token.{InfixToken, Token}
import ast.{Statement, Program}
import env.Env
import cats.Show
import cats.implicits.toShow
import evaluator.builtin.Builtin

enum Object:
  def unwrap: MonkeyPrimitiveType = this match
    case Object.ReturnValue(obj: MonkeyPrimitiveType) => obj
    case obj: MonkeyPrimitiveType                     => obj

  case Int(value: scala.Int)
  case Str(value: String)
  case Arr(elems: Seq[Object])
  case HashMap(hashmap: Map[Object, Object])
  case Boolean(value: scala.Boolean)
  case ReturnValue(value: MonkeyPrimitiveType)
  case Function(params: Seq[Token.Ident], program: Program, env: Env)
  case BuiltinObj(builtin: Builtin)
  case Null

type MonkeyPrimitiveType = Object.Int | Object.Boolean | Object.Null.type | Object.Function | Object.Str |
  Object.BuiltinObj | Object.Arr | Object.HashMap

extension (item: MonkeyPrimitiveType)
  def getType: String = item match
    case Object.Int(_)            => "Int"
    case Object.Str(_)            => "String"
    case Object.Arr(_)            => "Array"
    case Object.HashMap(_)        => "HashMap"
    case Object.Boolean(_)        => "Boolean"
    case Object.Null              => "null"
    case Object.Function(_, _, _) => "function"
    case obj: Object.BuiltinObj   => "builtin function"

given Show[Object] with
  def show(obj: Object) = obj match
    case Object.Int(value)     => value.toString()
    case Object.Str(value)     => value
    case Object.Boolean(value) => value.toString()
    case Object.Arr(elems)     => s"[${elems.map(_.show).mkString(", ")}]"
    case Object.HashMap(hashmap) =>
      s"{${hashmap.toList.map(item => s"${item._1.show}: ${item._2.show}").mkString(", ")}}"
    case Object.ReturnValue(value) => value.asInstanceOf[Object].show
    case Object.Function(_, _, _)  => "function"
    case Object.BuiltinObj(_)      => "builtin"
    case Object.Null               => "null"
