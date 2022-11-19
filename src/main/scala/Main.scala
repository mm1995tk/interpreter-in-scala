import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, ParserError}
import ast.given
import obj.Object
import evaluator.{evalProgram, EvalError}
import env.Env
import obj.getValue

@main def main: Unit =
  println("\nWelcome to Monkey Language!");
  println("");
  val env = Env()

  repl(env)

def repl(env: Env): Env =
  print(">> ")
  val input = scala.io.StdIn.readLine()
  if (input == ":q") {
    println("");
    println("thanks for using!");
    println("");

    System.exit(0);
  }
  println("");

  val (e, v) = Parser(Lexer(input)).parseProgram()._2 match
    case Right(v) =>
      val (e1, v1) = evalProgram(v, env)
      e1 -> {
        v1 match
          case Right(obj) => obj.show
          case Left(err)  => err.show
      }
    case Left(e) => env -> e.show

  println(v)

  repl(e)

extension (obj: Object)
  def show = obj match
    case Object.Int(value)         => value
    case Object.Boolean(value)     => value
    case Object.ReturnValue(value) => value.getValue.getOrElse("null")
    case Object.Function(_, _, _)  => "function"
    case Object.Null               => "null"
