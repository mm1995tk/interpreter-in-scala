package lexer

import token.Token
import scala.annotation.tailrec

class Lexer private (input: String, cursor: Int):
  def getToken: (Lexer, Token) =
    val next = this.next
    this.getChar match
      case '=' =>
        if next.getChar == '=' then next.next -> Token.Eq
        else next -> Token.Assign

      case '!' =>
        if next.getChar == '=' then next.next -> Token.NotEq
        else next -> Token.Bang

      case ch: CodeLiteral => next -> ch.convertCharOfCodeToToken

      case ch if ch.isDigit => this.readNumber(this.advanceCursor)

      case ch if ch.isLetter => this.readIdentifier(this.advanceCursor)
      case 0                 => next -> Token.Eof
      case _                 => next -> Token.Illegal

  private def next = this.advanceCursor.skipWhitespace

  @tailrec
  private def skipWhitespace: Lexer =
    if this.getChar.isWhitespace then this.advanceCursor.skipWhitespace
    else this

  private def getChar =
    if this.cursor > this.input.length - 1 then 0.toChar
    else this.input.charAt(this.cursor)

  private def advanceCursor = new Lexer(this.input, this.cursor + 1)

  @tailrec
  private def readIdentifier(lexer: Lexer, relativePos: Int = 0): (Lexer, Token) =
    if !lexer.getChar.isLetter then
      lexer.skipWhitespace -> (input.substring(this.cursor, this.cursor + 1 + relativePos) match
        case "let"    => Token.Let
        case "return" => Token.Return
        case "if"     => Token.If
        case "else"   => Token.Else
        case "true"   => Token.True
        case "false"  => Token.False
        case "fn"     => Token.Function
        case others   => Token.Ident(others)
      )
    else this.readIdentifier(lexer.advanceCursor, relativePos + 1)

  @tailrec
  private def readNumber(lexer: Lexer, relativePos: Int = 0): (Lexer, Token.Int) =
    if !lexer.getChar.isDigit then
      lexer.skipWhitespace -> Token.Int {
        input.substring(this.cursor, this.cursor + 1 + relativePos).toInt
      }
    else this.readNumber(lexer.advanceCursor, relativePos + 1)

object Lexer:
  def apply(input: String): Lexer = new Lexer(input, 0).skipWhitespace

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
