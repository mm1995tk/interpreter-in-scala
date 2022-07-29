case class Lexer(input: String, cursor: Int = 0):
  def getToken =
    val next = this.next
    this.getChar match
      case '=' =>
        if next.getChar == '=' then (next.next, Token.EQ)
        else (next, Token.ASSIGN)
      case '!' =>
        if next.getChar == '=' then (next.next, Token.NotEQ)
        else (next, Token.BANG)
      case '+' => (next, Token.PLUS)
      case '(' => (next, Token.LPAREN)
      case ')' => (next, Token.RPAREN)
      case '{' => (next, Token.LBRACE)
      case '}' => (next, Token.RBRACE)
      case ',' => (next, Token.COMMA)
      case ';' => (next, Token.SEMICOLON)
      case 0   => (next, Token.EOF)
      case _   => (next, Token.ILLEGAL)

  private def next = this.advanceCursor.skipWhitespace
  // def next(i: Int = 0) = advanceCursorRecursive(i, this.advanceCursor)

  private def skipWhitespace: Lexer =
    if this.getChar.isAsciiWhitespace then this.advanceCursor.skipWhitespace
    else this

  private def getChar =
    if this.cursor > this.input.length - 1 then 0.toChar
    else this.input.charAt(this.cursor)

  private def advanceCursor = Lexer(this.input, this.cursor + 1)

  // private def advanceCursorRecursive(
  //     i: Int,
  //     item: Lexer
  // ): Lexer =
  //   if i == 0 then item.skipWhitespace
  //   else
  //     advanceCursorRecursive(
  //       i - 1,
  //       item.skipWhitespace.advanceCursor
  //     )

object Lexer:
  def from(input: String): Lexer = Lexer(input).skipWhitespace

extension (item: Char)
  def isAsciiWhitespace: Boolean = item match
    case ' ' | '\n' | '\t' | '\r' => true
    case _                        => false

  def isMaybeCharNeedToSkip = item match
    case '=' | '!' => true
    case _         => false
