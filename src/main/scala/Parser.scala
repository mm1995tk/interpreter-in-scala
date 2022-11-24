package parser

import ast.*
import lexer.*
import token.*
import scala.annotation.tailrec
import cats.data.State
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff._
import cats.data.{StateT, State}

// TODO: モナドトランスフォーマーを使って書き換え
// type EitherParserErrorOr[T] = Either[ParserError, T]
// type ParserM = StateT[EitherParserErrorOr, String, Program]

// def f: ParserM = ???
// def g: ParserM = for {
//   ff <- f
//   g <- StateT.get[EitherParserErrorOr, String]
//   j <- StateT.fromState[EitherParserErrorOr, String, Token](lexer.tokenize.map(Right(_)))
//   s <- StateT.lift[EitherParserErrorOr, String, Int](Right(3))
// } yield Seq()

// def h =
//   g.runA("aaaasss")

sealed case class Parser private (
    private val input: String,
    private val curToken: Token,
    private val peekToken: Token
):

  def parseProgram(token: Token = Token.Eof): ParserState[Program] =
    Parser.parseProgram(this, token)

  private def parseStatement: ParserState[Statement] =
    this.curToken match
      case Token.Let    => this.parseLetStatement
      case Token.Return => this.parseReturnStatement
      case _            => this.parseExprStatement

  private def parseLetStatement: ParserState[Statement] =
    this.peekToken match
      case ident @ Token.Ident(_) =>
        val nextParser = this.next
        if nextParser.peekToken != Token.Assign then
          nextParser -> Left(ParserError.UnexpectedToken(nextParser.peekToken, Token.Assign))
        else
          val (latestParser, eitherExpr) = nextParser.next.next.parseExpr(Precedence.Lowest)
          latestParser.next -> eitherExpr.map(Statement.Let(ident, _))
      case token => this -> Left(ParserError.UnexpectedToken(token, Token.Ident("variable names")))

  private def parseReturnStatement: ParserState[Statement] =
    val (latestParser, eitherExpr) = this.next.parseExpr(Precedence.Lowest)
    latestParser.next -> eitherExpr.map(Statement.Return(_))

  private def parseExprStatement: ParserState[Statement] =
    val (parser, result) = this.parseExpr(Precedence.Lowest)
    val nextParser = if parser.peekToken.equals(Token.Semicolon) then parser.next else parser
    nextParser -> result.map(Statement.Expr(_))

  private def parseExpr(precedence: Precedence): ParserState[Expr] =
    val leftExp: ParserState[Expr] = this.curToken match
      case Token.Null         => this -> Right(Expr.Null)
      case t @ Token.Ident(_) => this -> Right(Expr.Ident(t))
      case t @ Token.Int(_)   => this -> Right(Expr.Int(t))
      case Token.If           => this.parseIfExpr
      case Token.LeftParen    => this.parseGroupExpr
      case Token.Function     => this.paraseFnLiteral
      case t: PrefixToken     => this.parsePrefixExpr(t)
      case t: BoolToken       => this -> Right(Expr.Bool(t))
      case _                  => this -> Left(ParserError.NotImplemented)

    @tailrec def go(item: ParserState[Expr]): ParserState[Expr] =
      val (parser, either) = item
      either match
        case Left(err) => parser -> Left(err)
        case Right(expr) =>
          parser.peekToken match
            case infix: InfixToken if precedence < getInfixPrecedence(infix) =>
              go(parser.parseInfixExpr(expr))
            case _ => item
    end go

    go(leftExp)
  end parseExpr

  private def paraseFnLiteral: ParserState[Expr] =
    if !this.peekToken.equals(Token.LeftParen) then
      return this -> Left(ParserError.UnexpectedToken(this.peekToken, Token.LeftParen))

    val (parser, params) = this.parseFnParams
    val (parser2, body) = {
      if !parser.peekToken.equals(Token.LeftBrace) then
        return parser -> Left(ParserError.UnexpectedToken(parser.peekToken, Token.LeftBrace))
      parser.next.next.parseProgram(Token.RightBrace)
    }
    parser2 -> {
      for {
        p <- params
        b <- body
      } yield Expr.Fn(p, b)
    }

  private def parseFnParams: ParserState[Seq[Expr.Ident]] =
    if !this.peekToken.equals(Token.LeftParen) then
      return this -> Left(ParserError.UnexpectedToken(this.peekToken, Token.LeftParen))

    val n = this.next
    if n.peekToken.equals(Token.RightParen) then return n -> Right(Seq())

    @tailrec def rec(item: ParserState[Seq[Expr.Ident]]): ParserState[Seq[Expr.Ident]] =
      if !item._1.peekToken.equals(Token.Comma) then return item
      val parser = item._1.next.next
      parser.curToken match
        case t @ Token.Ident(_) => rec(parser -> item._2.map(_.concat(Seq(Expr.Ident(t)))))
        case t                  => parser -> Left(ParserError.UnexpectedToken(t, Token.Ident("vars")))
    end rec

    val nn = n.next
    nn.curToken match
      case t @ Token.Ident(_) =>
        val (p, expr) = rec(nn -> Right(Seq(Expr.Ident(t))))
        p.next -> expr
      case t => nn -> Left(ParserError.UnexpectedToken(t, Token.Ident("vars")))
  end parseFnParams

  private def parsePrefixExpr(ident: PrefixToken): ParserState[Expr] =
    val (latestParser, expr) = this.next.parseExpr(Precedence.Prefix)
    latestParser -> expr.map(Expr.Prefix(ident, _))

  private def parseInfixExpr(left: Expr): ParserState[Expr] =
    this.peekToken match
      case leftParen @ Token.LeftParen =>
        left match
          case fn: (Expr.Fn | Expr.Ident) =>
            val (p, expr) = this.parseCallArgs
            p -> expr.map(Expr.Call(fn, _))
          case _ => this -> Left(ParserError.NotImplemented)
      case infixToken: InfixToken =>
        val (p, expr) = this.next.next.parseExpr(getInfixPrecedence(infixToken))
        p -> expr.map(Expr.Infix(infixToken, left, _))
      case other => this -> Left(ParserError.UnexpectedToken(other, Token.Ident("infix token")))

  private def parseCallArgs: ParserState[Seq[Expr]] =
    val n = this.next
    if n.peekToken.equals(Token.RightParen) then return n -> Right(Seq())

    @tailrec def rec(item: ParserState[Seq[Expr]]): ParserState[Seq[Expr]] =
      if !item._1.peekToken.equals(Token.Comma) then return item
      val (parser, expr) = item._1.next.next.parseExpr(Precedence.Lowest)

      val result = for {
        seq <- item._2
        expr <- expr
      } yield seq :+ expr

      rec(parser -> result)
    end rec

    val nn = n.next
    val (a, b) = nn.parseExpr(Precedence.Lowest)
    val (p, v) = rec(a -> b.map(Seq(_)))
    p.next -> v

  end parseCallArgs

  private def parseBlockStatement: ParserState[Program] =
    if !this.peekToken.equals(Token.LeftBrace) then
      return this -> Left(ParserError.UnexpectedToken(this.peekToken, Token.LeftBrace))
    val (parser, eitherStmts) = this.next.next.parseProgram(Token.RightBrace)
    parser.next -> eitherStmts

  private def parseIfExpr: ParserState[Expr] =
    if !this.peekToken.equals(Token.LeftParen) then
      return this -> Left(ParserError.UnexpectedToken(this.peekToken, Token.LeftParen))

    val (parser, notValidatedEitherCond) = this.next.parseExpr(Precedence.Lowest)
    val validatedEitherCond = notValidatedEitherCond.flatMap(expr =>
      val isRightParenToken = parser.curToken.equals(Token.RightParen)
      if isRightParenToken && parser.peekToken.equals(Token.LeftBrace) then Right(expr)
      else if isRightParenToken then Left(ParserError.UnexpectedToken(parser.curToken, Token.RightParen))
      else Left(ParserError.UnexpectedToken(parser.peekToken, Token.LeftBrace))
    )

    val (parsedByIf, eitherConseq) = parser.parseBlockStatement

    val (parsedByElse, eitherAlter): ParserState[Option[Program]] =
      if parsedByIf.curToken.equals(Token.Else) then
        val (parser, alter) = parsedByIf.parseBlockStatement
        parser -> alter.map(Some(_))
      else parsedByIf -> Right(None)

    parsedByElse -> {
      for {
        cond <- validatedEitherCond
        conseq <- eitherConseq
        alter <- eitherAlter
      } yield Expr.If(cond, conseq, alter)
    }
  end parseIfExpr

  private def parseGroupExpr: ParserState[Expr] =
    val (latestParser, expr) = this.next.parseExpr(Precedence.Lowest)
    latestParser.peekToken match
      case Token.RightParen => latestParser.next -> expr
      case other            => this -> Left(ParserError.UnexpectedToken(other, Token.RightParen))

  private def next: Parser =
    val (state, nextToken) = lexer.tokenize.run(this.input).value
    this.copy(input = state, curToken = this.peekToken, peekToken = nextToken)

end Parser

object Parser:
  def apply(input: String) =
    val (state, (cur, peek)) = (for {
      a <- lexer.tokenize
      b <- lexer.tokenize
    } yield (a, b)).run(input).value

    new Parser(state, cur, peek)

  @tailrec private def parseProgram(
      parser: Parser,
      endToken: Token,
      acc: Either[ParserError, Program] = Right(Seq())
  ): ParserState[Program] =
    if parser.curToken.equals(endToken) then return parser -> acc

    val parsed = parser.parseStatement
    parsed._2 match
      case Right(stmt) => parseProgram(parsed._1.next, endToken, acc.map(_ :+ stmt))
      case Left(e)     => parsed._1 -> Left(e)
  end parseProgram

private type ParserState[T] = (Parser, Either[ParserError, T])

enum ParserError:
  def show: String = this match
    case NotImplemented => "not impl"
    case UnexpectedToken(obtained, expected) =>
      s"expected token is \"${expected.showLiteral}\", but obatained is \"${obtained.showLiteral}\""

  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

private enum Precedence:
  def <(target: Precedence) = this.ordinal < target.ordinal
  def >(target: Precedence) = this.ordinal > target.ordinal
  case Lowest, Equals, LessOrGreater, Sum, Product, Prefix, Call

private def getInfixPrecedence(token: InfixToken): Precedence = token match
  case Token.Plus      => Precedence.Sum
  case Token.Minus     => Precedence.Sum
  case Token.Asterisk  => Precedence.Product
  case Token.Slash     => Precedence.Product
  case Token.Lt        => Precedence.LessOrGreater
  case Token.Gt        => Precedence.LessOrGreater
  case Token.Eq        => Precedence.Equals
  case Token.NotEq     => Precedence.Equals
  case Token.LeftParen => Precedence.Call
