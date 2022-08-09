package lexer

import token.Token
import scala.annotation.tailrec

class Lexer private (input: String, cursor: Int):
  def getToken: (Lexer, Token) =
    val next = this.next
    this.getChar match
      case '=' =>
        if next.getChar == '=' then next.next -> Token.EQ
        else next -> Token.ASSIGN

      case '!' =>
        if next.getChar == '=' then next.next -> Token.NotEQ
        else next -> Token.BANG

      case ch: CodeLiteral => next -> ch.convertCharOfCodeToToken

      case ch if ch.isDigit => this.readNumber(next)

      case ch if ch.isLetter => this.readIdentifier(next)
      case 0                 => next -> Token.EOF
      case _                 => next -> Token.ILLEGAL

  private def next = this.advanceCursor.skipWhitespace

  @tailrec
  private def skipWhitespace: Lexer =
    if this.getChar.isAsciiWhitespace then this.advanceCursor.skipWhitespace
    else this

  private def getChar =
    if this.cursor > this.input.length - 1 then 0.toChar
    else this.input.charAt(this.cursor)

  private def advanceCursor = new Lexer(this.input, this.cursor + 1)

  @tailrec
  private def readIdentifier(next: Lexer, relativePos: Int = 0): (Lexer, Token) =
    if !next.getChar.isLetter then
      next.skipWhitespace -> (input.substring(this.cursor, this.cursor + 1 + relativePos) match
        case "let"    => Token.LET
        case "return" => Token.RETURN
        case "if"     => Token.IF
        case "else"   => Token.ELSE
        case "true"   => Token.TRUE
        case "false"  => Token.FALSE
        case "fn"     => Token.FUNCTION
        case others   => Token.IDENT(others)
      )
    else this.readIdentifier(next.advanceCursor, relativePos + 1)

  @tailrec
  private def readNumber(next: Lexer, relativePos: Int = 0): (Lexer, Token.INT) =
    if !next.getChar.isDigit then
      next.skipWhitespace -> Token.INT {
        input.substring(this.cursor, this.cursor + 1 + relativePos).toInt
      }
    else this.readNumber(next.advanceCursor, relativePos + 1)

object Lexer:
  def apply(input: String): Lexer = new Lexer(input, 0).skipWhitespace

extension (item: Char)
  private def isAsciiWhitespace: Boolean = item match
    case ' ' | '\n' | '\t' | '\r' => true
    case _                        => false

private type CodeLiteral = '+' | '-' | '/' | '*' | '<' | '>' | '(' | ')' | '{' | '}' | ',' | ';'
extension (item: CodeLiteral)
  private def convertCharOfCodeToToken =
    item match
      case '+' => Token.PLUS
      case '-' => Token.MINUS
      case '/' => Token.SLASH
      case '*' => Token.ASTERISK
      case '<' => Token.LT
      case '>' => Token.GT
      case '(' => Token.LPAREN
      case ')' => Token.RPAREN
      case '{' => Token.LBRACE
      case '}' => Token.RBRACE
      case ',' => Token.COMMA
      case ';' => Token.SEMICOLON
