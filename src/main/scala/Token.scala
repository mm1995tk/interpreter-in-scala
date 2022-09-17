package token

enum Token:
  case Illegal,
    Eof,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return
  case Ident(value: String)
  case Int(value: scala.Int)

type PrefixToken = Token.Minus.type | Token.Bang.type

type InfixToken =
  Token.Plus.type | Token.Minus.type | Token.Asterisk.type | Token.Slash.type | Token.Lt.type |
    Token.Gt.type | Token.Eq.type | Token.NotEq.type | Token.LeftParen.type

type BoolToken = Token.True.type | Token.False.type
