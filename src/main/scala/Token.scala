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
    Return,
    Null
  case Ident(value: String)
  case Int(value: scala.Int)

type PrefixToken = Token.Minus.type | Token.Bang.type

type InfixToken =
  Token.Plus.type | Token.Minus.type | Token.Asterisk.type | Token.Slash.type | Token.Lt.type |
    Token.Gt.type | Token.Eq.type | Token.NotEq.type | Token.LeftParen.type

type BoolToken = Token.True.type | Token.False.type

extension (token: Token)
  def showLiteral: String = token match
    case Token.Null                 => "null"
    case Token.Illegal              => 0.toChar.toString()
    case Token.Eof                  => 0.toChar.toString()
    case Token.Assign               => "="
    case Token.Plus                 => "+"
    case Token.Minus                => "-"
    case Token.Bang                 => "!"
    case Token.Asterisk             => "*"
    case Token.Slash                => "/"
    case Token.Lt                   => "<"
    case Token.Gt                   => ">"
    case Token.Eq                   => "=="
    case Token.NotEq                => "!="
    case Token.Comma                => ","
    case Token.Semicolon            => ";"
    case Token.LeftParen            => "("
    case Token.RightParen           => ")"
    case Token.LeftBrace            => "{"
    case Token.RightBrace           => "}"
    case Token.Function             => "fn"
    case Token.Let                  => "let"
    case Token.True                 => "true"
    case Token.False                => "false"
    case Token.If                   => "if"
    case Token.Else                 => "else"
    case Token.Return               => "return"
    case Token.Ident(value: String) => value
    case Token.Int(value: Int)      => value.toString()
