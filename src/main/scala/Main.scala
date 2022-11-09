import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, showErr}
import ast.given
import obj.Object

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

  val parser = Parser(Lexer(input))

  evaluator.Evaluator(parser.parseProgram()._2) match
    case Right(obj) => println(obj.show)
    case Left(v)    => println(showErr(v))

  repl

extension (obj: Object)
  def show = obj match
    case Object.Int(value)     => value
    case Object.Boolean(value) => value
    case Object.ReturnValue(value) => value
    case Object.Null           => "null"
