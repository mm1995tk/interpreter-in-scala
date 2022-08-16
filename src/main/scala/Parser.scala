package parser

import cats.data.{Reader, Kleisli}
import ast.*
import lexer.*
import token.*
import cats.data.State
import scala.annotation.tailrec

sealed case class Parser private (
    private val lexer: Lexer,
    private val curToken: Token,
    private val peekToken: Token
):

  def parseProgram: Either[ParserErrors, Program] = Parser.parseProgram(this)

  private def parseStatement: ParserState[Statement] =
    this.curToken match
      case Token.LET    => this.parseLetStatement
      case Token.RETURN => this.parseReturnStatement
      case _            => this.parseExprStatement

  private def parseLetStatement: ParserState[Statement] =
    this.peekToken match
      case ident @ Token.IDENT(str) =>
        val nextParser = this.next
        if nextParser.peekToken != Token.ASSIGN then
          nextParser -> Left(ParserError.UnexpectedToken(nextParser.peekToken, Token.ASSIGN))
        else
          val (latestParser, eitherExpr) = nextParser.next.next.parseExpr(Precedence.LOWEST)
          latestParser.next -> eitherExpr.map(Statement.LET(ident, _))
      case token => this -> Left(ParserError.UnexpectedToken(token, Token.IDENT("variable names")))

  private def parseReturnStatement: ParserState[Statement] =
    this.skipToSemicolon -> Right(Statement.RETURN { Expr.IDENT(Token.IDENT("dummy")) })

  private def parseExprStatement: ParserState[Statement] =
    val (parser, result) = this.parseExpr(Precedence.LOWEST)
    val nextParser = if parser.peekToken.equals(Token.SEMICOLON) then parser.next else parser
    nextParser -> result.map(Statement.EXPR(_))

  private def parseExpr(precedence: Precedence): ParserState[Expr] =
    val leftExp: ParserState[Expr] = this.curToken match
      case Token.IDENT(_) => this.parseIdentifier
      case Token.INT(_)   => this.parseIntLiteral
      case _: PrefixToken => this.parsePrefixExpr
      case _              => this -> Left(ParserError.NotImplemented)

    @tailrec def go(item: ParserState[Expr]): ParserState[Expr] =
      val (parser, either) = item
      either match
        case Left(err) => parser -> Left(err)
        case Right(expr) =>
          parser.peekToken match
            case infix: InfixToken if precedence < getInfixPrecedence(infix) =>
              go(parser.next.parseInfixExpr(expr))
            case _ => item
    end go

    go(leftExp)
  end parseExpr

  private def parseIdentifier: ParserState[Expr] = this -> {
    this.curToken match
      case ident @ Token.IDENT(_) => Right(Expr.IDENT(ident))
      case other                  => Left(ParserError.UnexpectedToken(other, Token.IDENT("any identity")))
  }

  private def parseIntLiteral: ParserState[Expr] = this -> {
    this.curToken match
      case ident @ Token.INT(_) => Right(Expr.INT(ident))
      case other                => Left(ParserError.UnexpectedToken(other, Token.IDENT("any integer")))
  }

  private def parsePrefixExpr: ParserState[Expr] = this.curToken match
    case ident: PrefixToken =>
      val (latestParser, expr) = this.next.parseExpr(Precedence.PREFIX)
      latestParser -> expr.map(Expr.PREFIX(ident, _))
    case other => this -> Left(ParserError.UnexpectedToken(other, Token.IDENT("prefix token")))

  private def parseInfixExpr(left: Expr): ParserState[Expr] =
    this.curToken match
      case infixToken: InfixToken =>
        val (latestParser, expr) = this.next.parseExpr(getInfixPrecedence(infixToken))
        latestParser -> expr.map(Expr.INFIX(infixToken, left, _))
      case other => this -> Left(ParserError.UnexpectedToken(other, Token.IDENT("infix token")))

  private def next: Parser =
    val (nextLexer, token) = lexer.getToken
    this.copy(lexer = nextLexer, curToken = this.peekToken, peekToken = token)

  @tailrec
  private def skipToSemicolon: Parser =
    if this.curToken == Token.SEMICOLON then this else this.next.skipToSemicolon

object Parser:
  def apply(lexer: Lexer) =
    val (secondLexer, curToken) = lexer.getToken
    val (thirdLexer, peekToken) = secondLexer.getToken
    new Parser(thirdLexer, curToken, peekToken)

  @tailrec
  private def parseProgram(
      parser: Parser,
      result: Either[ParserErrors, Program] = Right(Seq())
  ): Either[ParserErrors, Program] =
    if parser.curToken.equals(Token.EOF) then result
    else
      val parsed = parser.parseStatement
      parsed._2 match
        case Right(stmt) => parseProgram(parsed._1.next, result.map(_ :+ stmt))
        case Left(err) =>
          parseProgram(
            parsed._1.next,
            Left {
              result match
                case Left(seq) => seq :+ err
                case _         => Seq(err)
            }
          )

private type ParserState[T] = (Parser, Either[ParserError, T])

private enum ParserError:
  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

private type ParserErrors = Seq[ParserError]

private enum Precedence:
  def <(target: Precedence) = this.ordinal < target.ordinal
  def >(target: Precedence) = this.ordinal > target.ordinal
  case LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL

private def getInfixPrecedence(token: InfixToken): Precedence = token match
  case Token.PLUS     => Precedence.SUM
  case Token.MINUS    => Precedence.SUM
  case Token.ASTERISK => Precedence.PRODUCT
  case Token.SLASH    => Precedence.PRODUCT
  case Token.LT       => Precedence.LESSGREATER
  case Token.GT       => Precedence.LESSGREATER
  case Token.EQ       => Precedence.EQUALS
  case Token.NotEQ    => Precedence.EQUALS
