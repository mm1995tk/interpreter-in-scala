package token

enum Token:
  case ILLEGAL,
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NotEQ,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
  case IDENT(value: String)
  case INT(value: Int)

type PrefixToken = Token.MINUS.type | Token.BANG.type

type InfixToken =
  Token.PLUS.type | Token.MINUS.type | Token.ASTERISK.type | Token.SLASH.type | Token.LT.type |
    Token.GT.type | Token.EQ.type | Token.NotEQ.type