case class Lexer(input: String, cursor: Int = 0):
  def getToken: (Lexer, Token) =
    val next = this.next
    this.getChar match
      case '=' =>
        if next.getChar == '=' then (next.next, Token.EQ)
        else next -> Token.ASSIGN

      case '!' =>
        if next.getChar == '=' then (next.next, Token.NotEQ)
        else next -> Token.BANG

      case ch: CodeLiteral => next -> ch.convertCharOfCodeToToken

      case ch if ch.isDigit =>
        val (lexer, token) = this.readNumber()
        lexer.skipWhitespace -> token

      case ch if ch.isLetter => this.getKeyWordToken
      case 0                 => next -> Token.EOF
      case _                 => next -> Token.ILLEGAL

  private def getKeyWordToken =
    val (lexer, ident) = this.readIdentifier()
    lexer.skipWhitespace -> {
      ident match
        case "let"    => Token.LET
        case "return" => Token.RETURN
        case "if"     => Token.IF
        case "else"   => Token.ELSE
        case "true"   => Token.TRUE
        case "false"  => Token.FALSE
        case "fn"     => Token.FUNCTION
        case ch       => Token.IDENT(ch)
    }

  private def next = this.advanceCursor.skipWhitespace

  private def skipWhitespace: Lexer =
    if this.getChar.isAsciiWhitespace then this.advanceCursor.skipWhitespace
    else this

  private def getChar =
    if this.cursor > this.input.length - 1 then 0.toChar
    else this.input.charAt(this.cursor)

  private def advanceCursor = Lexer(this.input, this.cursor + 1)

  private def readIdentifier(relativePos: Int = 0, next: Lexer = this.next): (Lexer, String) =
    if !next.getChar.isLetter then
      next -> input.substring(this.cursor, this.cursor + relativePos + 1)
    else this.readIdentifier(relativePos + 1, next.advanceCursor)

  private def readNumber(relativePos: Int = 0, next: Lexer = this.next): (Lexer, Token.INT) =
    if !next.getChar.isDigit then
      next -> Token.INT { input.substring(this.cursor, this.cursor + relativePos + 1).toInt }
    else this.readNumber(relativePos + 1, next.advanceCursor)

object Lexer:
  def from(input: String): Lexer = Lexer(input).skipWhitespace

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
