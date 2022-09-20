package lexer

import token.Token
import scala.annotation.tailrec

case class Lexer(input: String, cursor: Int)
object Lexer:
  def apply(input: String): Lexer = skipWhitespace(Lexer(input, 0))

def getToken(lexer: Lexer): (Lexer, Token) =
  val n = next(lexer)
  getChar(lexer) match
    case '=' =>
      if getChar(n) == '=' then next(n) -> Token.Eq
      else n -> Token.Assign

    case '!' =>
      if getChar(n) == '=' then next(n) -> Token.NotEq
      else n -> Token.Bang

    case ch: CodeLiteral => n -> ch.convertCharOfCodeToToken

    case ch if ch.isDigit => readNumber(advanceCursor(lexer))

    case ch if ch.isLetter => readIdentifier(advanceCursor(lexer))
    case 0                 => n -> Token.Eof
    case _                 => n -> Token.Illegal

private def next = skipWhitespace compose advanceCursor

private def getChar(lexer: Lexer) =
  if lexer.cursor > lexer.input.length - 1 then 0.toChar
  else lexer.input.charAt(lexer.cursor)

private def advanceCursor(lexer: Lexer) = lexer.copy(cursor = lexer.cursor + 1)

@tailrec private def skipWhitespace(lexer: Lexer): Lexer =
  if !getChar(lexer).isWhitespace then lexer
  else skipWhitespace(advanceCursor(lexer))

@tailrec private def readNumber(lexer: Lexer, relativePos: Int = 0): (Lexer, Token.Int) =
  if !getChar(lexer).isDigit then
    skipWhitespace(lexer) -> Token.Int {
      lexer.input.substring(lexer.cursor - relativePos - 1, lexer.cursor).toInt
    }
  else readNumber(advanceCursor(lexer), relativePos + 1)

@tailrec private def readIdentifier(lexer: Lexer, relativePos: Int = 0): (Lexer, Token) =
  if !getChar(lexer).isLetter then
    skipWhitespace(lexer) -> (lexer.input.substring(lexer.cursor - relativePos - 1, lexer.cursor) match
      case "let"    => Token.Let
      case "return" => Token.Return
      case "if"     => Token.If
      case "else"   => Token.Else
      case "true"   => Token.True
      case "false"  => Token.False
      case "fn"     => Token.Function
      case others   => Token.Ident(others)
    )
  else readIdentifier(advanceCursor(lexer), relativePos + 1)

private type CodeLiteral = '+' | '-' | '/' | '*' | '<' | '>' | '(' | ')' | '{' | '}' | ',' | ';'
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
      case ',' => Token.Comma
      case ';' => Token.Semicolon
