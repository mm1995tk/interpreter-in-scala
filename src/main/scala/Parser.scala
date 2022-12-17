package parser

import ast.*
import lexer.*
import token.{*, given}
import cats.data.{StateT, State}
import cats.Show
import cats.implicits.*
import cats.kernel.Order
import cats.Alternative

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
  let <- Parser.expect(Token.Let)
  ident <- Parser.nextToken.flatMap {
    case t: Token.Ident => Parser.pure(t)
    case obtained       => Parser.pureErr(ParserError.UnexpectedTokenText(obtained, "name of vars"))
  }
  assign <- Parser.expect(Token.Assign)
  expr <- parseExpr()
  semicolon <- Parser.expect(Token.Semicolon)
} yield Statement.Let(ident, expr)

private def parseReturnStatement: Parser[Statement] =
  Parser.expect(Token.Return) *> parseExpr().map(Statement.Return(_)) <* Parser.expect(Token.Semicolon)

private def parseExprStatement: Parser[Statement] = for {
  expr <- parseExpr()
  optionalSemicolon <- Parser.previewToken.flatMap {
    case Token.Semicolon => Parser.nextToken.map(Some.apply)
    case _               => Parser.pure(None)
  }
} yield Statement.Expr(expr, !optionalSemicolon.isEmpty)

private def parseBlockStatement: Parser[Program] =
  Parser.expect(Token.LeftBrace) *> parse(Seq(), Token.RightBrace) <* Parser.expect(Token.RightBrace)

private def parseExpr(precedence: Precedence = Precedence.Lowest): Parser[Expr] =
  for {
    left <- Parser.previewToken.flatMap {
      case Token.Null         => Parser.nextToken.as(Expr.Null)
      case t @ Token.Ident(_) => Parser.nextToken.as(Expr.Ident(t))
      case t @ Token.Int(_)   => Parser.nextToken.as(Expr.Int(t))
      case t @ Token.Str(_)   => Parser.nextToken.as(Expr.Str(t))
      case t: BoolToken       => Parser.nextToken.as(Expr.Bool(t))
      case t: PrefixToken     => parsePrefixExpr
      case Token.If           => parseIfExpr
      case Token.LeftParen    => parseGroupExpr
      case Token.LeftBracket  => parseArr
      case Token.Function     => paraseFnLiteral
      case obtained => Parser.pureErr(ParserError.UnexpectedTokenText(obtained, "atomic expression"))
    }
    expr <- parseFoldExprFromLeft(left, precedence)
  } yield expr

private def parseFoldExprFromLeft(left: Expr, precedence: Precedence): Parser[Expr] =
  Parser.previewToken.flatMap {
    case t: InfixToken if precedence < getInfixPrecedence(t) =>
      for {
        right <- Parser.nextToken *> parseExpr(getInfixPrecedence(t))
        result <- parseFoldExprFromLeft(Expr.Infix(t, left, right), precedence)
      } yield result
    case Token.LeftParen =>
      for {
        sym <- left match
          case fn: (Expr.Fn | Expr.Ident | Expr.Call | Expr.If) => Parser.pure(fn)
          case _ =>
            Parser.pureErr(ParserError.Message("need to expression that returns a function when evaluated"))
        params <- parseArgs(parseExpr(), Token.RightParen)
        expr = Expr.Call(sym, params)
        result <- parseFoldExprFromLeft(expr, precedence)
      } yield result
    case Token.LeftBracket =>
      for {
        arrLit <- left match
          case _: (Expr.Int | Expr.Str | Expr.Null.type | Expr.Bool) =>
            Parser.pureErr(ParserError.Message("need to expression that returns a array when evaluated"))
          case expr => Parser.pure(expr)
        index <- Parser.nextToken *> parseExpr() <* Parser.nextToken
        expr = Expr.Index(arrLit, index)
        result <- parseFoldExprFromLeft(expr, precedence)
      } yield result
    case _ => Parser.pure(left)
  }

private def parseArr: Parser[Expr] = parseArgs[Expr](parseExpr(), Token.RightBracket).map(Expr.Arr(_))

private def parsePrefixExpr: Parser[Expr] = for {
  prefixToken: PrefixToken <- Parser.nextToken.flatMap {
    case t: PrefixToken => Parser.pure(t)
    case obtained       => Parser.pureErr(ParserError.UnexpectedTokenText(obtained, "prefix token"))
  }
  expr <- parseExpr(Precedence.Prefix)
} yield Expr.Prefix(prefixToken, expr)

private def parseIfExpr: Parser[Expr] = for {
  ifToken <- Parser.expect(Token.If)
  cond <- parseGroupExpr
  consequence <- parseBlockStatement
  alter <- Parser.previewToken.flatMap {
    case Token.Else => Parser.nextToken *> parseBlockStatement.map(Some(_))
    case _          => Parser.pure(None)
  }
} yield Expr.If(cond, consequence, alter)

private def paraseFnLiteral: Parser[Expr] = for {
  fn <- Parser.expect(Token.Function)
  args <- parseArgs(
    Parser.nextToken.flatMap {
      case t: Token.Ident => Parser.pure(Expr.Ident(t))
      case obtained       => Parser.pureErr(ParserError.UnexpectedTokenText(obtained, "name of vars"))
    }: Parser[Expr.Ident],
    Token.RightParen
  )
  body <- parseBlockStatement
} yield Expr.Fn(args, body)

private def parseArgs[T](
    parserOfArg: Parser[T],
    endToken: Token.RightParen.type | Token.RightBracket.type,
    args: Seq[T] = Seq()
): Parser[Seq[T]] =
  def go[T](parserOfArg: Parser[T], args: Seq[T]): Parser[Seq[T]] = for {
    arg <- parserOfArg
    updatedArgs: Seq[T] = args :+ arg
    args <- Parser.previewToken.flatMap {
      case Token.Comma             => Parser.nextToken *> go(parserOfArg, updatedArgs)
      case t if t.equals(endToken) => Parser.pure(updatedArgs)
      case obtained =>
        Parser.pureErr(ParserError.UnexpectedTokenText(obtained, s"',' or '${endToken.show}'"))
    }
  } yield args
  end go

  endToken match
    case Token.RightParen => parseBetweenParen(go(parserOfArg, args))
    case Token.RightBracket =>
      Parser.expect(Token.LeftBracket) *> go(parserOfArg, args) <* Parser.expect(Token.RightBracket)

private def parseGroupExpr: Parser[Expr] = parseBetweenParen(parseExpr())

private def parseBetweenParen[T](parser: Parser[T]): Parser[T] =
  Parser.expect(Token.LeftParen) *> parser <* Parser.expect(Token.RightParen)

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

  def expect(expected: Token): Parser[Unit] = Parser.nextToken.flatMap {
    case obtained if expected.ordinal == obtained.ordinal => Parser.pure(())
    case obtained => Parser.pureErr(ParserError.UnexpectedToken(obtained, expected))
  }

enum ParserError:
  case Message(content: String)
  case UnexpectedToken(obtained: Token, expected: Token)
  case UnexpectedTokenText(obtained: Token, expected: String)

given Show[ParserError] with
  def show(t: ParserError): String =
    def unexpected(obtained: Token, expected: String) =
      val obtainedStr = obtained.show
      s"expected token is $expected, but obatained is '${
          if obtainedStr.trim().isEmpty() then "nothing" else obtainedStr
        }'"
    t match
      case ParserError.Message(content)                        => content
      case ParserError.UnexpectedToken(obtained, expected)     => unexpected(obtained, expected.show)
      case ParserError.UnexpectedTokenText(obtained, expected) => unexpected(obtained, expected)

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
