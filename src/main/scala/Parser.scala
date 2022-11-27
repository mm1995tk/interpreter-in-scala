package parser

import ast.*
import lexer.*
import token.*
import cats.data.{StateT, State}
import cats.Show
import cats.implicits.*

type EitherParserErrorOr[T] = Either[ParserError, T]
type Parser[T] = StateT[EitherParserErrorOr, String, T]

def parseProgram: Parser[Program] = parse()

private def parse(program: Program = Seq(), endToken: Token = Token.Eof): Parser[Program] =
  for {
    stmt <- parseStatement
    appended = program :+ stmt
    previewToken <- Parser.previewToken
    program <-
      if previewToken.equals(endToken) then Parser.pure(appended)
      else parse(appended, endToken)
  } yield program

private def parseStatement: Parser[Statement] = Parser.previewToken.flatMap {
  case Token.Let    => parseLetStatement
  case Token.Return => parseReturnStatement
  case _            => parseExprStatement
}

private def parseLetStatement: Parser[Statement] = for {
  let <- Parser.nextToken.flatMap {
    case t @ Token.Let => Parser.pure(t)
    case _             => Utils.fromParserErr(???)
  }
  ident <- Parser.nextToken.flatMap {
    case t: Token.Ident => Parser.pure(t)
    case _              => Utils.fromParserErr(???)
  }
  assign <- Parser.nextToken.flatMap {
    case t @ Token.Assign => Parser.pure(t)
    case _                => Utils.fromParserErr(???)
  }
  expr <- parseExpr()
  semicolon <- Parser.nextToken.flatMap {
    case t @ Token.Semicolon => Parser.pure(t)
    case _                   => Utils.fromParserErr(???)
  }
} yield Statement.Let(ident, expr)

private def parseReturnStatement: Parser[Statement] = for {
  returnStmt <- Parser.nextToken.flatMap {
    case t @ Token.Return => Parser.pure(t)
    case _                => Utils.fromParserErr(???)
  }
  expr <- parseExpr()
  semicolon <- Parser.nextToken.flatMap {
    case t @ Token.Semicolon => Parser.pure(t)
    case _                   => Utils.fromParserErr(???)
  }
} yield Statement.Return(expr)

private def parseExprStatement: Parser[Statement] = for {
  expr <- parseExpr()
  token <- Parser.previewToken
  result <- token match
    case t: InfixToken => parseExprStatement
    case _             => Parser.pure(Statement.Expr(expr))
  preview <- Parser.previewToken
  _ <- if preview.equals(Token.Semicolon) then Parser.nextToken else Parser.pure(preview)
} yield result

private def parseBlockStatement: Parser[Program] = for {
  lBrace <- Parser.nextToken.flatMap {
    case t @ Token.LeftBrace => Parser.pure(t)
    case _                   => Utils.fromParserErr(???)
  }
  program <- parse(Seq(), Token.RightBrace)
  rBrace <- Parser.nextToken.flatMap {
    case t @ Token.RightBrace => Parser.pure(t)
    case _                    => Utils.fromParserErr(???)
  }
} yield program

private def parseExpr(precedence: Precedence = Precedence.Lowest): Parser[Expr] =
  def recurInfix(left: Expr): Parser[Expr] = for {
    previewToken <- Parser.previewToken

    expr <- previewToken match
      case infix: InfixToken if precedence < getInfixPrecedence(infix) =>
        parseInfixExpr(left).flatMap(recurInfix)
      case _ => Parser.pure(left)

  } yield expr
  for {
    token <- Parser.previewToken
    left <- token match
      case Token.Null         => Parser.nextToken.map(_ => Expr.Null)
      case t @ Token.Ident(_) => Parser.nextToken.map(_ => Expr.Ident(t))
      case t @ Token.Int(_)   => Parser.nextToken.map(_ => Expr.Int(t))
      case t: BoolToken       => Parser.nextToken.map(_ => Expr.Bool(t))
      case t: PrefixToken     => parsePrefixExpr
      case Token.If           => parseIfExpr
      case Token.LeftParen    => parseGroupExpr
      case Token.Function     => paraseFnLiteral
      case _                  => Utils.fromParserErr(ParserError.NotImplemented)
    result <- recurInfix(left)
  } yield result

private def parsePrefixExpr: Parser[Expr] = for {
  prefixToken: PrefixToken <- Parser.nextToken.flatMap {
    case t: PrefixToken => Parser.pure(t)
    case _              => Utils.fromParserErr(???)
  }
  expr <- parseExpr(Precedence.Prefix)
} yield Expr.Prefix(prefixToken, expr)

private def parseInfixExpr(left: Expr): Parser[Expr] = for {
  infixToken: InfixToken <- Parser.nextToken.flatMap {
    case t: InfixToken => Parser.pure(t)
    case _             => Utils.fromParserErr(???)
  }

  result <- infixToken match
    case t @ Token.LeftParen => parseCallFnExpr(left)
    case _ => parseExpr(getInfixPrecedence(infixToken)).map(Expr.Infix(infixToken, left, _))
} yield result

private def parseGroupExpr: Parser[Expr] = for {
  lParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.LeftParen) => Parser.pure(t)
    case _                              => Utils.fromParserErr(???)
  }
  result <- parseExpr()
  rParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.RightParen) => Parser.pure(t)
    case _                               => Utils.fromParserErr(???)
  }
} yield result

private def paraseFnLiteral: Parser[Expr] = for {
  fn <- Parser.nextToken.flatMap {
    case t @ Token.Function => Parser.pure(t)
    case _                  => Utils.fromParserErr(???)
  }
  lParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.LeftParen) => Parser.pure(t)
    case _                              => Utils.fromParserErr(???)
  }
  args <- paraseFnParams()
  rParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.RightParen) => Parser.pure(t)
    case _                               => Utils.fromParserErr(???)
  }
  body <- parseBlockStatement
} yield Expr.Fn(args, body)

private def paraseFnParams(args: Seq[Expr.Ident] = Seq()): Parser[Seq[Expr.Ident]] = for {
  arg <- Parser.nextToken.flatMap {
    case t: Token.Ident => Parser.pure(Expr.Ident(t))
    case _              => Utils.fromParserErr(???)
  }: Parser[Expr.Ident]

  updatedArgs: Seq[Expr.Ident] = args :+ arg
  previewToken <- Parser.previewToken
  result <- previewToken match
    case Token.Comma      => Parser.nextToken *> paraseFnParams(updatedArgs)
    case Token.RightParen => Parser.pure(updatedArgs)
    case _                => Utils.fromParserErr(???)
} yield result

private def parseCallFnExpr(left: Expr): Parser[Expr] = for {
  fn <- left match
    case fn: (Expr.Fn | Expr.Ident) => Parser.pure(fn)
    case _                          => Utils.fromParserErr(???)

  params <- parseCallArgs()
  rParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.RightParen) => Parser.pure(t)
    case _                               => Utils.fromParserErr(???)
  }
} yield Expr.Call(fn, params)

private def parseCallArgs(args: Seq[Expr] = Seq()): Parser[Seq[Expr]] = for {
  arg <- parseExpr()
  updatedArgs: Seq[Expr] = args :+ arg
  previewToken <- Parser.previewToken

  result <- previewToken match
    case Token.Comma      => Parser.nextToken *> parseCallArgs(updatedArgs)
    case Token.RightParen => Parser.pure(updatedArgs)
    case _                => Utils.fromParserErr(???)
} yield result

private def parseIfExpr: Parser[Expr] = for {
  ifToken <- Parser.nextToken.flatMap {
    case t @ Token.If => Parser.pure(t)
    case _            => Utils.fromParserErr(???)
  }
  cond <- parseGroupExpr
  consequence <- parseBlockStatement
  preview <- Parser.previewToken
  alter <- preview match
    case Token.Else =>
      for {
        elseClause <- Parser.nextToken
        program <- parseBlockStatement
      } yield Some(program)
    case _ => Parser.pure(None)
} yield Expr.If(cond, consequence, alter)

object Parser:
  def pure[T](t: T): Parser[T] = StateT.pure(t)

  def nextToken: Parser[Token] =
    StateT.fromState[EitherParserErrorOr, String, Token](lexer.tokenize.map(Right(_)))

  def previewToken: Parser[Token] = for {
    str <- StateT.get
    token <- nextToken
    _ <- StateT.set(str)
  } yield token

object Utils:
  def fromParserErr[T](err: ParserError) = Utils.liftParser[T](Left(err))
  def liftParser[T] = StateT.lift[EitherParserErrorOr, String, T]

enum ParserError:
  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

given Show[ParserError] with
  def show(t: ParserError): String = t match
    case ParserError.NotImplemented => "not impl"
    case ParserError.UnexpectedToken(obtained, expected) =>
      s"expected token is \"${expected.showLiteral}\", but obatained is \"${obtained.showLiteral}\""

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
