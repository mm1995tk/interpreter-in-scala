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
      case Token.Ident(_)  => this.parseIdentifier
      case Token.Int(_)    => this.parseIntLiteral
      case Token.If        => this.parseIfExpr
      case Token.LeftParen => this.parseGroupExpr
      case Token.Function  => this.paraseFnLiteral
      case _: PrefixToken  => this.parsePrefixExpr
      case _: BoolToken    => this.parseBoolExpr
      case _               => this -> Left(ParserError.NotImplemented)

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
        val (p, v) = rec(nn -> Right(Seq(Expr.Ident(t))))
        p.next -> v
      case t => nn -> Left(ParserError.UnexpectedToken(t, Token.Ident("vars")))
  end parseFnParams

  private def parsePrefixExpr: ParserState[Expr] = this.curToken match
    case ident: PrefixToken =>
      val (latestParser, expr) = this.next.parseExpr(Precedence.Prefix)
      latestParser -> expr.map(Expr.Prefix(ident, _))
    case other => this -> Left(ParserError.UnexpectedToken(other, Token.Ident("prefix token")))

  private def parseInfixExpr(left: Expr): ParserState[Expr] =
    this.peekToken match
      case leftParen @ Token.LeftParen =>
        left match
          case fn: (Expr.Fn | Expr.Ident) =>
            val (p, expr) = this.parseCallArgs
            p -> expr.map(item => Expr.Call(fn, item))
          case _ => this -> Left(ParserError.NotImplemented)
      case infixToken: InfixToken =>
        val (latestParser, expr) = this.next.next.parseExpr(getInfixPrecedence(infixToken))
        latestParser -> expr.map(Expr.Infix(infixToken, left, _))
      case other => this -> Left(ParserError.UnexpectedToken(other, Token.Ident("infix token")))

  private def parseCallArgs: ParserState[Seq[Expr]] =
    val n = this.next
    if n.peekToken.equals(Token.RightParen) then return n -> Right(Seq())

    @tailrec def rec(item: ParserState[Seq[Expr]]): ParserState[Seq[Expr]] =
      if !item._1.peekToken.equals(Token.Comma) then return item
      val parser = item._1.next.next
      val tmp = parser.parseExpr(Precedence.Lowest)
      rec(tmp._1 -> {
        for {
          a <- item._2
          b <- tmp._2
        } yield a.appended(b)
      })
    end rec

    val nn = n.next
    val (a, b) = nn.parseExpr(Precedence.Lowest)
    val (p, v) = rec(a -> b.map(Seq(_)))
    p.next -> v

  end parseCallArgs

  private def parseBoolExpr: ParserState[Expr] = this -> {
    this.curToken match
      case token: BoolToken => Right(Expr.Bool(token))
      case other            => Left(ParserError.UnexpectedToken(other, Token.Ident("bool token")))
  }

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
    val (nextLexer, token) = lexer.getToken
    this.copy(lexer = nextLexer, curToken = this.peekToken, peekToken = token)

end Parser

object Parser:
  def apply(lexer: Lexer) =
    val (secondLexer, curToken) = lexer.getToken
    val (thirdLexer, peekToken) = secondLexer.getToken
    new Parser(thirdLexer, curToken, peekToken)

  @tailrec private def parseProgram(
      parser: Parser,
      endToken: Token,
      acc: Either[ParserErrors, Program] = Right(Seq())
  ): ParserState[Program] =
    if parser.curToken.equals(endToken) then return parser -> acc

    val parsed = parser.parseStatement
    parsed._2 match
      case Right(stmt) => parseProgram(parsed._1.next, endToken, acc.map(_ :+ stmt))
      case Left(err) =>
        parseProgram(
          parsed._1.next,
          endToken,
          Left {
            acc match
              case Left(seq) => seq.concat(err.lift)
              case _         => err.lift
          }
        )
  end parseProgram

private type ParserState[T] = (Parser, Either[ParserError | ParserErrors, T])

private enum ParserError:
  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

private type ParserErrors = List[ParserError]

extension (item: ParserError | ParserErrors)
  def lift: ParserErrors = item match
    case err: ParserError   => List(err)
    case errs: ParserErrors => errs

def showErr(item: ParserError | ParserErrors): String = item match
  case err: ParserError   => err.show
  case errs: ParserErrors => errs.map(_.show).mkString(", ")

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

extension (p: ParserError)
  def show: String = p match
    case ParserError.UnexpectedToken(obtained, expected) =>
      s"expected token is \"${expected.showLiteral}\", but obatained is \"${obtained.showLiteral}\""
    case _ => "not impl"
