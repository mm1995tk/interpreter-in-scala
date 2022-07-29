import scala.sys.process.processInternal
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

  loop(Lexer.from(input))
  repl

def loop(lexer: Lexer): Unit =
  val (next, token) = lexer.getToken
  if (token == Token.EOF) {
    println("");
    return
  }
  println(token)
  loop(next)
