import scala.sys.process.processInternal
import lexer.Lexer
import token.Token

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

  loop(Lexer(input))
  repl

def loop(lexer: Lexer): Unit =
  val (next, token) = lexer.getToken
  if (token == Token.Eof) {
    println("");
    return
  }
  println(token)
  loop(next)
