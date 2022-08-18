package parser

import ast.*
import lexer.*
import token.*
import scala.annotation.tailrec

sealed case class Parser private (
    private val lexer: Lexer,
    private val curToken: Token,
    private val peekToken: Token
):

  def parseProgram: Either[ParserErrors, Program] = Parser.parseProgram(this)

  private def parseStatement: ParserState[Statement] =
    this.curToken match
      case Token.Let    => this.parseLetStatement
      case Token.Return => this.parseReturnStatement
      case _            => this.parseExprStatement

  private def parseLetStatement: ParserState[Statement] =
    this.peekToken match
      case ident @ Token.Ident(str) =>
        val nextParser = this.next
        if nextParser.peekToken != Token.Assign then
          nextParser -> Left(ParserError.UnexpectedToken(nextParser.peekToken, Token.Assign))
        else
          val (latestParser, eitherExpr) = nextParser.next.next.parseExpr(Precedence.LOWEST)
          latestParser.next -> eitherExpr.map(Statement.Let(ident, _))
      case token => this -> Left(ParserError.UnexpectedToken(token, Token.Ident("variable names")))

  private def parseReturnStatement: ParserState[Statement] =
    val (latestParser, eitherExpr) = this.next.parseExpr(Precedence.LOWEST)
    latestParser.next -> eitherExpr.map(Statement.Return(_))

  private def parseExprStatement: ParserState[Statement] =
    val (parser, result) = this.parseExpr(Precedence.LOWEST)
    val nextParser = if parser.peekToken.equals(Token.Semicolon) then parser.next else parser
    nextParser -> result.map(Statement.Expr(_))

  private def parseExpr(precedence: Precedence): ParserState[Expr] =
    val leftExp: ParserState[Expr] = this.curToken match
      case Token.Ident(_)  => this.parseIdentifier
      case Token.Int(_)    => this.parseIntLiteral
      case _: PrefixToken  => this.parsePrefixExpr
      case _: BoolToken    => this.parseBool
      case Token.LeftParen => this.parseGroupExpr
      case _               => this -> Left(ParserError.NotImplemented)

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
      case ident @ Token.Ident(_) => Right(Expr.Ident(ident))
      case other                  => Left(ParserError.UnexpectedToken(other, Token.Ident("any identity")))
  }

  private def parseIntLiteral: ParserState[Expr] = this -> {
    this.curToken match
      case ident @ Token.Int(_) => Right(Expr.Int(ident))
      case other                => Left(ParserError.UnexpectedToken(other, Token.Ident("any integer")))
  }

  private def parsePrefixExpr: ParserState[Expr] = this.curToken match
    case ident: PrefixToken =>
      val (latestParser, expr) = this.next.parseExpr(Precedence.Prefix)
      latestParser -> expr.map(Expr.Prefix(ident, _))
    case other => this -> Left(ParserError.UnexpectedToken(other, Token.Ident("prefix token")))

  private def parseInfixExpr(left: Expr): ParserState[Expr] =
    this.curToken match
      case infixToken: InfixToken =>
        val (latestParser, expr) = this.next.parseExpr(getInfixPrecedence(infixToken))
        latestParser -> expr.map(Expr.Infix(infixToken, left, _))
      case other => this -> Left(ParserError.UnexpectedToken(other, Token.Ident("infix token")))

  private def parseBool: ParserState[Expr] = this -> {
    this.curToken match
      case token: BoolToken => Right(Expr.Bool(token))
      case other            => Left(ParserError.UnexpectedToken(other, Token.Ident("bool token")))
  }

  private def parseGroupExpr: ParserState[Expr] =
    val (latestParser, expr) = this.next.parseExpr(Precedence.LOWEST)
    latestParser.peekToken match
      case Token.RightParen => latestParser.next -> expr
      case other            => this -> Left(ParserError.UnexpectedToken(other, Token.RightParen))

  private def next: Parser =
    val (nextLexer, token) = lexer.getToken
    this.copy(lexer = nextLexer, curToken = this.peekToken, peekToken = token)

  @tailrec
  private def skipToSemicolon: Parser =
    if this.curToken == Token.Semicolon then this else this.next.skipToSemicolon

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
    if parser.curToken.equals(Token.Eof) then result
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
  case LOWEST, EqUALS, LESSGREATER, SUM, PRODUCT, Prefix, CALL

private def getInfixPrecedence(token: InfixToken): Precedence = token match
  case Token.Plus     => Precedence.SUM
  case Token.Minus    => Precedence.SUM
  case Token.Asterisk => Precedence.PRODUCT
  case Token.Slash    => Precedence.PRODUCT
  case Token.Lt       => Precedence.LESSGREATER
  case Token.Gt       => Precedence.LESSGREATER
  case Token.Eq       => Precedence.EqUALS
  case Token.NotEq    => Precedence.EqUALS
