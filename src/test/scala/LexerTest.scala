class LexerTest extends munit.FunSuite {
  test("next token") {
    val input = " = +(  ){ },;p"
    val lexer = Lexer.from(input)

    for (
      (token, index) <- Seq(
        Token.ASSIGN,
        Token.PLUS,
        Token.LPAREN,
        Token.RPAREN,
        Token.LBRACE,
        Token.RBRACE,
        Token.COMMA,
        Token.SEMICOLON,
        Token.ILLEGAL
      ).zipWithIndex
    ) {

      assertEquals(lexer.next(index).get.getNextToken.get, token)
    }
  }
}
