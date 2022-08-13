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

  def parseProgram = Parser.parseProgram(this)

  private def parseStatement: (Parser, Either[ParserError, Statement]) =
    this.curToken match
      case Token.LET => this.parseLetStatement
      case Token.RETURN =>
        val (p, stmt) = this.parseReturnStatement
        p -> Right(stmt)
      case _ => this -> Left(ParserError.NotImplemented)

  private def parseLetStatement: (Parser, Either[ParserError, Statement]) =
    this.peekToken match
      case Token.IDENT(str) =>
        val nextParser = this.next
        if nextParser.peekToken != Token.ASSIGN then
          nextParser -> Left(ParserError.UnexpectedToken(nextParser.peekToken, Token.ASSIGN))
        else
          val ident: Ident = Ident(str)
          nextParser.skipToSemicolon -> Right(Statement.LET(ident, Expr.IDENT(ident)))
      case token => this -> Left(ParserError.UnexpectedToken(token, Token.IDENT("variable names")))

  private def parseReturnStatement: (Parser, Statement) =
    this.skipToSemicolon -> Statement.RETURN { Expr.IDENT(Ident("dummy")) }

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

enum ParserError:
  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

type ParserErrors = Seq[ParserError]
