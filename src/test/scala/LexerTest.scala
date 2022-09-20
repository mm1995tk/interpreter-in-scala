import token.Token.*
import lexer.Lexer
class LexerTest extends munit.FunSuite {
  test("ソースコード") {
    var l = Lexer(scala.io.Source.fromResource("sample.monkey").getLines.mkString)
    val m = new Lexer("", 0)

    for (
      token <- Seq(
        Let,
        Ident("five"),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Ident("ten"),
        Assign,
        Int(10),
        Semicolon,
        Let,
        Ident("add"),
        Assign,
        Function,
        LeftParen,
        Ident("x"),
        Comma,
        Ident("y"),
        RightParen,
        LeftBrace,
        Ident("x"),
        Plus,
        Ident("y"),
        Semicolon,
        RightBrace,
        Semicolon,
        Let,
        Ident("result"),
        Assign,
        Ident("add"),
        LeftParen,
        Ident("five"),
        Comma,
        Ident("ten"),
        RightParen,
        Semicolon,
        Bang,
        Minus,
        Slash,
        Asterisk,
        Int(5),
        Semicolon,
        Int(5),
        Lt,
        Int(10),
        Gt,
        Int(5),
        Semicolon,
        If,
        LeftParen,
        Int(5),
        Lt,
        Int(10),
        RightParen,
        LeftBrace,
        Return,
        True,
        Semicolon,
        RightBrace,
        Else,
        LeftBrace,
        Return,
        False,
        Semicolon,
        RightBrace,
        Int(10),
        Eq,
        Int(10),
        Semicolon,
        Int(10),
        NotEq,
        Int(9),
        Semicolon
      )
    ) {
      val (nextLexer, curToken) = lexer.getToken(l)
      assertEquals(curToken, token)
      l = nextLexer
    }
    val (nextLexer, curToken) = lexer.getToken(l)
    assertEquals(curToken, Eof)
  }

}
