import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, showErr}
import ast.given

@main def main: Unit =
  println("\nWelcome to Monkey Language!");
  println("");
  repl

def repl: Unit =
  print(">> ")
  val input = scala.io.StdIn.readLine()
  if (input == "exit;") {
    println("");
    println("thanks for using!");
    println("");

    System.exit(0);
  }
  println("");

  val parser = Parser(Lexer(input))

  parser.parseProgram()._2.map(_.toStr) match
    case Right(v) => println(v)
    case Left(v)  => println(showErr(v))

  repl
