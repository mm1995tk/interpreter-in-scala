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

  private def parseStatement: (Parser, Option[Statement]) =
    this.curToken match
      case Token.LET => this.parseLetStatement
      case Token.RETURN =>
        val (p, stmt) = this.parseReturnStatement
        p -> Some(stmt)
      case _ => this -> None

  private def parseLetStatement: (Parser, Option[Statement]) =
    this.peekToken match
      case Token.IDENT(str) =>
        val nextParser = this.next
        if nextParser.peekToken != Token.ASSIGN then nextParser -> None
        else
          val ident: Ident = Ident(str)
          nextParser.skipToSemicolon -> Some(Statement.LET(ident, Expr.IDENT(ident)))
      case _ => this -> None

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
  private def parseProgram(parser: Parser, result: Program = Seq()): Program =
    if parser.curToken == Token.EOF then result
    else
      val parsed = parser.parseStatement
      parsed._2 match
        case Some(stmt) => parseProgram(parsed._1.next, result :+ stmt)
        case _          => result

enum ParserError:
  case UnexpextedToken(obtained: Token, expexted: Token)
