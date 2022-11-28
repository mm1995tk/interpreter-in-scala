package parser

import ast.*
import lexer.*
import token.{*, given}
import cats.data.{StateT, State}
import cats.Show
import cats.implicits.*
import cats.kernel.Order

type Parser[T] = StateT[EitherParserErrorOr, String, T]
type EitherParserErrorOr[T] = Either[ParserError, T]

def parseProgram: Parser[Program] = parse()

private def parse(program: Program = Seq(), endToken: Token = Token.Eof): Parser[Program] =
  for {
    stmt <- parseStatement
    appended = program :+ stmt
    program <- Parser.previewToken.flatMap {
      case t if t.equals(endToken) => Parser.pure(appended)
      case _                       => parse(appended, endToken)
    }
  } yield program

private def parseStatement: Parser[Statement] = Parser.previewToken.flatMap {
  case Token.Let    => parseLetStatement
  case Token.Return => parseReturnStatement
  case _            => parseExprStatement
}

private def parseLetStatement: Parser[Statement] = for {
  let <- Parser.nextToken.flatMap {
    case t @ Token.Let => Parser.pure(t)
    case _             => Parser.pureErr(???)
  }
  ident <- Parser.nextToken.flatMap {
    case t: Token.Ident => Parser.pure(t)
    case _              => Parser.pureErr(???)
  }
  assign <- Parser.nextToken.flatMap {
    case t @ Token.Assign => Parser.pure(t)
    case _                => Parser.pureErr(???)
  }
  expr <- parseExpr()
  semicolon <- Parser.nextToken.flatMap {
    case t @ Token.Semicolon => Parser.pure(t)
    case _                   => Parser.pureErr(???)
  }
} yield Statement.Let(ident, expr)

private def parseReturnStatement: Parser[Statement] = for {
  returnToken <- Parser.nextToken.flatMap {
    case t @ Token.Return => Parser.pure(t)
    case _                => Parser.pureErr(???)
  }
  expr <- parseExpr()
  semicolon <- Parser.nextToken.flatMap {
    case t @ Token.Semicolon => Parser.pure(t)
    case _                   => Parser.pureErr(???)
  }
} yield Statement.Return(expr)

private def parseExprStatement: Parser[Statement] = for {
  expr <- parseExpr()
  optionalSemicolon <- Parser.previewToken.flatMap {
    case Token.Semicolon => Parser.nextToken.map(Some.apply)
    case _               => Parser.pure(None)
  }
} yield Statement.Expr(expr, !optionalSemicolon.isEmpty)

private def parseBlockStatement: Parser[Program] = for {
  lBrace <- Parser.nextToken.flatMap {
    case t @ Token.LeftBrace => Parser.pure(t)
    case _                   => Parser.pureErr(???)
  }
  program <- parse(Seq(), Token.RightBrace)
  rBrace <- Parser.nextToken.flatMap {
    case t @ Token.RightBrace => Parser.pure(t)
    case _                    => Parser.pureErr(???)
  }
} yield program

private def parseExpr(precedence: Precedence = Precedence.Lowest): Parser[Expr] =
  for {
    left <- Parser.previewToken.flatMap {
      case Token.Null         => Parser.nextToken.map(_ => Expr.Null)
      case t @ Token.Ident(_) => Parser.nextToken.map(_ => Expr.Ident(t))
      case t @ Token.Int(_)   => Parser.nextToken.map(_ => Expr.Int(t))
      case t: BoolToken       => Parser.nextToken.map(_ => Expr.Bool(t))
      case t: PrefixToken     => parsePrefixExpr
      case Token.If           => parseIfExpr
      case Token.LeftParen    => parseGroupExpr
      case Token.Function     => paraseFnLiteral
      case _                  => Parser.pureErr(ParserError.NotImplemented)
    }
    result <- parseRight(left, precedence)
  } yield result

private def parseRight(left: Expr, precedence: Precedence): Parser[Expr] =
  Parser.previewToken.flatMap {
    case t: InfixToken if precedence < getInfixPrecedence(t) =>
      for {
        right <- Parser.nextToken *> parseExpr(getInfixPrecedence(t))
        result <- parseRight(Expr.Infix(t, left, right), precedence)
      } yield result
    case Token.LeftParen =>
      for {
        sym <- left match
          case fn: (Expr.Fn | Expr.Ident | Expr.Call) => Parser.pure(fn)
          case _                                      => Parser.pureErr(ParserError.NotImplemented)
        params <- parseArgs(parseExpr())
        expr = Expr.Call(sym, params)
        result <- parseRight(expr, precedence)
      } yield result
    case _ => Parser.pure(left)
  }

private def parsePrefixExpr: Parser[Expr] = for {
  prefixToken: PrefixToken <- Parser.nextToken.flatMap {
    case t: PrefixToken => Parser.pure(t)
    case _              => Parser.pureErr(???)
  }
  expr <- parseExpr(Precedence.Prefix)
} yield Expr.Prefix(prefixToken, expr)

private def parseIfExpr: Parser[Expr] = for {
  ifToken <- Parser.nextToken.flatMap {
    case t @ Token.If => Parser.pure(t)
    case _            => Parser.pureErr(???)
  }
  cond <- parseGroupExpr
  consequence <- parseBlockStatement
  alter <- Parser.previewToken.flatMap {
    case Token.Else =>
      for {
        elseClause <- Parser.nextToken
        program <- parseBlockStatement
      } yield Some(program)
    case _ => Parser.pure(None)
  }
} yield Expr.If(cond, consequence, alter)

private def paraseFnLiteral: Parser[Expr] = for {
  fn <- Parser.nextToken.flatMap {
    case t @ Token.Function => Parser.pure(t)
    case _                  => Parser.pureErr(???)
  }
  args <- parseArgs(Parser.nextToken.flatMap {
    case t: Token.Ident => Parser.pure(Expr.Ident(t))
    case _              => Parser.pureErr(???)
  }: Parser[Expr.Ident])
  body <- parseBlockStatement
} yield Expr.Fn(args, body)

private def parseArgs[T](parserOfArg: Parser[T], args: Seq[T] = Seq()): Parser[Seq[T]] =
  def go[T](parserOfArg: Parser[T], args: Seq[T]): Parser[Seq[T]] = for {
    arg <- parserOfArg
    updatedArgs: Seq[T] = args :+ arg
    args <- Parser.previewToken.flatMap {
      case Token.Comma      => Parser.nextToken *> go(parserOfArg, updatedArgs)
      case Token.RightParen => Parser.pure(updatedArgs)
      case _                => Parser.pureErr(???)
    }
  } yield args
  end go
  parseBetweenParen(go(parserOfArg, args))

private def parseGroupExpr: Parser[Expr] = parseBetweenParen(parseExpr())

private def parseBetweenParen[T](parser: Parser[T]): Parser[T] = for {
  lParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.LeftParen) => Parser.pure(t)
    case _                              => Parser.pureErr(???)
  }
  main <- parser
  rParen <- Parser.nextToken.flatMap {
    case t if t.equals(Token.RightParen) => Parser.pure(t)
    case _                               => Parser.pureErr(???)
  }
} yield main

private object Parser:
  def pure[T](t: T): Parser[T] = StateT.pure(t)

  def pureErr[T](err: ParserError): Parser[T] = StateT.lift[EitherParserErrorOr, String, T](Left(err))

  def nextToken: Parser[Token] =
    StateT.fromState[EitherParserErrorOr, String, Token](lexer.tokenize.map(Right(_)))

  def previewToken: Parser[Token] = for {
    str <- StateT.get
    token <- nextToken
    _ <- StateT.set(str)
  } yield token

enum ParserError:
  case NotImplemented
  case UnexpectedToken(obtained: Token, expexted: Token)

given Show[ParserError] with
  def show(t: ParserError): String = t match
    case ParserError.NotImplemented => "not impl"
    case ParserError.UnexpectedToken(obtained, expected) =>
      s"expected token is \"${expected.show}\", but obatained is \"${obtained.show}\""

private enum Precedence:
  case Lowest, Equals, LessOrGreater, Sum, Product, Prefix, Call

given Order[Precedence] with
  def compare(x: Precedence, y: Precedence): Int = x.ordinal - y.ordinal

private def getInfixPrecedence(token: InfixToken): Precedence = token match
  case Token.Plus     => Precedence.Sum
  case Token.Minus    => Precedence.Sum
  case Token.Asterisk => Precedence.Product
  case Token.Slash    => Precedence.Product
  case Token.Lt       => Precedence.LessOrGreater
  case Token.Gt       => Precedence.LessOrGreater
  case Token.Eq       => Precedence.Equals
  case Token.NotEq    => Precedence.Equals
