@main def repl: Unit =
  val lexer = Lexer.ap("app")
  println(lexer.get_cur)
  println(lexer.read_char.read_char.get_cur)
