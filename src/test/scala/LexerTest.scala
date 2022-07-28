class LexerTest extends munit.FunSuite {
  test("単一文字で形成されるトークン") {
    val input = " = +(  ){ },=;p"
    var lexer = Lexer.from(input)

    for (
      (token, index) <- Seq(
        Token.ASSIGN,
        Token.PLUS,
        Token.LPAREN,
        Token.RPAREN,
        Token.LBRACE,
        Token.RBRACE,
        Token.COMMA,
        Token.ASSIGN,
        Token.SEMICOLON,
        Token.ILLEGAL
      ).zipWithIndex
    ) {
      val (nextLexer, curToken) = lexer.getToken

      assertEquals(curToken, token)
      lexer = nextLexer
    }
    val (nextLexer, curToken) = lexer.getToken

    assertEquals(curToken, Token.EOF)
    lexer = nextLexer

  }
  test("==を含む") {
    val input = " === +(  ){ },;p"
    var lexer = Lexer.from(input)

    for (
      (token, index) <- Seq(
        Token.EQ,
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
      val (nextLexer, curToken) = lexer.getToken

      assertEquals(curToken, token)
      lexer = nextLexer
    }
    val (nextLexer, curToken) = lexer.getToken

    assertEquals(curToken, Token.EOF)
    lexer = nextLexer

  }

  test("！=を含む") {
    val input = " != +( ==0 ){ },;p"
    var lexer = Lexer.from(input)

    for (
      (token, index) <- Seq(
        Token.NotEQ,
        Token.PLUS,
        Token.LPAREN,
        Token.EQ,
        Token.ILLEGAL,
        Token.RPAREN,
        Token.LBRACE,
        Token.RBRACE,
        Token.COMMA,
        Token.SEMICOLON,
        Token.ILLEGAL
      ).zipWithIndex
    ) {
      val (nextLexer, curToken) = lexer.getToken

      assertEquals(curToken, token)
      lexer = nextLexer
    }
    val (nextLexer, curToken) = lexer.getToken

    assertEquals(curToken, Token.EOF)
    lexer = nextLexer

  }
}
