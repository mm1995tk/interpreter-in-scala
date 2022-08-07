import token.Token.*
import lexer.Lexer
class LexerTest extends munit.FunSuite {
  test("ソースコード") {
    var lexer = Lexer(scala.io.Source.fromResource("sample.monkey").getLines.mkString)

    for (
      (token, index) <- Seq(
        LET,
        IDENT("five"),
        ASSIGN,
        INT(5),
        SEMICOLON,
        LET,
        IDENT("ten"),
        ASSIGN,
        INT(10),
        SEMICOLON,
        LET,
        IDENT("add"),
        ASSIGN,
        FUNCTION,
        LPAREN,
        IDENT("x"),
        COMMA,
        IDENT("y"),
        RPAREN,
        LBRACE,
        IDENT("x"),
        PLUS,
        IDENT("y"),
        SEMICOLON,
        RBRACE,
        SEMICOLON,
        LET,
        IDENT("result"),
        ASSIGN,
        IDENT("add"),
        LPAREN,
        IDENT("five"),
        COMMA,
        IDENT("ten"),
        RPAREN,
        SEMICOLON,
        BANG,
        MINUS,
        SLASH,
        ASTERISK,
        INT(5),
        SEMICOLON,
        INT(5),
        LT,
        INT(10),
        GT,
        INT(5),
        SEMICOLON,
        IF,
        LPAREN,
        INT(5),
        LT,
        INT(10),
        RPAREN,
        LBRACE,
        RETURN,
        TRUE,
        SEMICOLON,
        RBRACE,
        ELSE,
        LBRACE,
        RETURN,
        FALSE,
        SEMICOLON,
        RBRACE,
        INT(10),
        EQ,
        INT(10),
        SEMICOLON,
        INT(10),
        NotEQ,
        INT(9),
        SEMICOLON
      ).zipWithIndex
    ) {
      val (nextLexer, curToken) = lexer.getToken
      assertEquals(curToken, token)
      lexer = nextLexer
    }
    val (nextLexer, curToken) = lexer.getToken
    assertEquals(curToken, EOF)
  }
}
