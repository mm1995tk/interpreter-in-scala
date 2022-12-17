package lexer

import token.Token
import cats.data.State
import cats.implicits.*

type Lexer = State[String, Token]

def tokenize: Lexer = next.flatMap {
  case char: ('=' | '!')     => twoCharLexer(char)
  case char: '"'             => stringLexer()
  case char: CodeLiteral     => State.pure(char.convertCharOfCodeToToken)
  case char if char.isDigit  => numberLexer(char.toString())
  case char if char.isLetter => identifierLexer(char.toString())
  case 0                     => State.pure(Token.Eof)
  case _                     => State.pure(Token.Illegal)
}

private def twoCharLexer(char0: Char): Lexer = for {
  state0 <- State.get[String]
  char1 <- next
  token <- char0 match
    case '=' =>
      if char1 == '=' then State.pure(Token.Eq)
      else State.set(state0).as(Token.Assign)

    case '!' =>
      if char1 == '=' then State.pure(Token.NotEq)
      else State.set(state0).as(Token.Bang)
} yield token

private def stringLexer(str: String = ""): Lexer = getChar.flatMap {
  case char: '"' => State.pure(Token.Str(str))
  case 0         => State.pure(Token.Str(str))
  case char      => stringLexer(s"$str$char")
}

private def numberLexer(n: String): Lexer = for {
  state0 <- State.get[String]
  char1 <- getChar
  token <-
    if char1.isDigit then numberLexer(s"${n}${char1}")
    else State.set(state0).as(Token.Int(n.toInt))
} yield token

private def identifierLexer(str: String): Lexer = for {
  state0 <- State.get[String]
  char0 <- getChar
  token <-
    if char0.isLetter then identifierLexer(s"${str}${char0}")
    else
      State
        .set(state0)
        .as {
          str match
            case "let"    => Token.Let
            case "return" => Token.Return
            case "if"     => Token.If
            case "else"   => Token.Else
            case "true"   => Token.True
            case "false"  => Token.False
            case "fn"     => Token.Function
            case "null"   => Token.Null
            case others   => Token.Ident(others)
        }
} yield token

private def getChar: State[String, Char] =
  State.get[String].flatMap { str =>
    if str.isEmpty()
    then State.pure(0.toChar)
    else State.set(str.substring(1)).as(str.charAt(0))
  }

private def skipWhitespace(char0: Char): State[String, Char] =
  if !char0.isWhitespace then State.pure(char0) else next

private def next: State[String, Char] = getChar.flatMap(skipWhitespace)

private type CodeLiteral = '+' | '-' | '/' | '*' | '<' | '>' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ';'
extension (item: CodeLiteral)
  private def convertCharOfCodeToToken =
    item match
      case '+' => Token.Plus
      case '-' => Token.Minus
      case '/' => Token.Slash
      case '*' => Token.Asterisk
      case '<' => Token.Lt
      case '>' => Token.Gt
      case '(' => Token.LeftParen
      case ')' => Token.RightParen
      case '{' => Token.LeftBrace
      case '}' => Token.RightBrace
      case '[' => Token.LeftBracket
      case ']' => Token.RightBracket
      case ',' => Token.Comma
      case ';' => Token.Semicolon
