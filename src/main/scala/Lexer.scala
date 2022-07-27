case class Lexer(input: String, cursor: Int = 0):
  def next(i: Int = 0) = advanceCursorRecursive(i, this.advanceCursor)

  def getNextToken = this.getChar map {
    case '=' => Token.ASSIGN
    case '+' => Token.PLUS
    case '(' => Token.LPAREN
    case ')' => Token.RPAREN
    case '{' => Token.LBRACE
    case '}' => Token.RBRACE
    case ',' => Token.COMMA
    case ';' => Token.SEMICOLON
    case _   => Token.ILLEGAL
  }

  private def skipWhitespace: Option[Lexer] =
    this.getChar match {
      case Some(ch) if ch.isAsciiWhitespace =>
        this.advanceCursor.flatMap(_.skipWhitespace)
      case _ => Some(this)
    }

  private def getChar =
    if this.cursor == 0 then None
    else Some(this.input.charAt(this.cursor - 1))

  private def advanceCursor =
    if this.hasNext then Some(Lexer(this.input, this.cursor + 1))
    else None

  private def len = this.input.length()
  private def hasNext = this.cursor < this.len

  private def advanceCursorRecursive(
      i: Int,
      item: Option[Lexer]
  ): Option[Lexer] =
    if i == 0 then item.flatMap(_.skipWhitespace)
    else
      advanceCursorRecursive(
        i - 1,
        item.flatMap(_.skipWhitespace.flatMap(_.advanceCursor))
      )

object Lexer {
  def from(input: String): Lexer = Lexer(input)
}

extension (item: Char)
  def isAsciiWhitespace: Boolean = item match
    case ' ' | '\n' | '\t' | '\r' => true
    case _                        => false
