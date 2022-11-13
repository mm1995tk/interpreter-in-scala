import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, showErr, ParserError, ParserErrors}
import ast.given
import obj.Object
import evaluator.evalProgram
import obj.EvalError

@main def main: Unit =
  println("\nWelcome to Monkey Language!");
  println("");
  repl

def repl: Unit =
  print(">> ")
  val input = scala.io.StdIn.readLine()
  if (input == ":exit") {
    println("");
    println("thanks for using!");
    println("");

    System.exit(0);
  }
  println("");

  Parser(input).parseProgram()._2.flatMap(evalProgram(_)) match
    case Right(obj) => println(obj.show)
    case Left(err) =>
      err match
        case t: EvalError => ???
        case _            => showErr(_)

  repl

extension (obj: Object)
  def show = obj match
    case Object.Int(value)         => value
    case Object.Boolean(value)     => value
    case Object.ReturnValue(value) => value
    case Object.Null               => "null"
