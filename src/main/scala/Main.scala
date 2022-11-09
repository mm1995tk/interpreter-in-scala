import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, showErr, ParserError, ParserErrors}
import ast.given
import obj.Object
import evaluator.Evaluator
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

  val parser = Parser(Lexer(input))

  val result = for {
    r <- parser.parseProgram()._2
    rr <- evaluator.Evaluator(r)
  } yield rr

  parser.parseProgram()._2.flatMap(Evaluator(_)) match
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
