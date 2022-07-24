class Lexer private (input: String, cur: Int, peek: Int) {

  def read_char: Lexer = Lexer(this.input, this.peek, this.peek + 1)
  def get_cur = this.cur

  def get_token = Token.LET
}

object Lexer {
  def ap(input: String): Lexer = Lexer(input, 0, 1)
}
