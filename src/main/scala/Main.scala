import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, ParserError, parseProgram}
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

  repl(env)

extension (obj: Object)
  def show = obj match
    case Object.Int(value)         => value
    case Object.Boolean(value)     => value
    case Object.ReturnValue(value) => value.getValue.getOrElse("null")
    case Object.Function(_, _, _)  => "function"
    case Object.Null               => "null"
