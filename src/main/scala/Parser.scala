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

type EitherParserErrorOr[T] = Either[ParserError, T]
type Parser = StateT[EitherParserErrorOr, String, Program]

def parseProgram: Parser = ???

// def f: ParserM = ???
// def g: ParserM = for {
//   ff <- f
//   g <- StateT.get[EitherParserErrorOr, String]
//   j <- StateT.fromState[EitherParserErrorOr, String, Token](lexer.tokenize.map(Right(_)))
//   s <- StateT.lift[EitherParserErrorOr, String, Int](Right(3))
// } yield Seq()

// def h =
//   g.runA("aaaasss")

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
