package parser

import lexer.Lexer
import ast.*
import token.*

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838382+1;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("let x = 5;", "let y = 10;", "let foobar = (838382 + 1);")))
      assertEquals(stmt.toStr, expected)

  }

  test("let文解析エラーのテスト") {
    val input = "let x  5;\nlet  = 10;\nlet 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram

    stmts match
      case Right(_) => assert(false)
      case Left(seq) =>
        assertEquals(
          seq.filterNot(_.equals(ParserError.NotImplemented)),
          Seq(
            ParserError.UnexpectedToken(Token.INT(5), Token.ASSIGN),
            ParserError.UnexpectedToken(
              Token.ASSIGN,
              Token.IDENT("variable names")
            ),
            ParserError.UnexpectedToken(
              Token.INT(838383),
              Token.IDENT("variable names")
            )
          )
        )

  }

  test("return文のテスト") {
    val input = "return  5;\nreturn 10;\nreturn 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for (stmt <- stmts)
      assert {
        stmt match
          case Statement.RETURN(_) => true
          case _                   => false
      }

  }

  test("識別子のテスト") {
    val input = "foobar;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val stmt = stmts.head
    assert(stmt match
      case Statement.EXPR(Expr.IDENT(Token.IDENT(value))) => value == "foobar"
      case _                                              => false
    )

  }

  test("整数リテラルのテスト") {
    val input = "5;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val stmt = stmts.head
    assert(stmt match
      case Statement.EXPR(Expr.INT(Token.INT(value))) => value == 5
      case _                                          => false
    )

  }

  test("前置演算子のテスト") {
    val input = "-15;!5;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 2 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val expecteds: Seq[(PrefixToken, Int)] = Seq((Token.MINUS, 15), (Token.BANG, 5))

    stmts.zip(expecteds).foreach { (stmt, expected) =>
      assertEquals(
        stmt,
        Statement.EXPR {
          Expr.PREFIX(expected._1, Expr.INT(Token.INT(expected._2)))
        }
      )
    }

  }

  test("中置演算子のテスト") {
    val input =
      Seq("5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 > 5;", "5 < 5;", "5 == 5;", "5 != 5;").mkString
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 8 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    assert(true)
    val expecteds: Seq[InfixToken] =
      Seq(Token.PLUS, Token.MINUS, Token.ASTERISK, Token.SLASH, Token.GT, Token.LT, Token.EQ, Token.NotEQ)

    val iter: Seq[(Statement, InfixToken)] = stmts.zip(expecteds)

    iter.foreach { item =>
      val stmt = item._1
      val expected: InfixToken = item._2
      assertEquals(
        stmt,
        Statement.EXPR {
          Expr.INFIX(expected, Expr.INT(Token.INT(5)), Expr.INT(Token.INT(5)))
        }
      )
    }

  }

  test("異なる優先度の演算子が混在するテスト") {
    for (test <- 異なる優先度の演算子が混在するテストのデータ)
      val parser = Parser(Lexer(test._1))
      val stmt = parser.parseProgram.getOrElse(Seq()).head

      assertEquals(stmt.toStr, test._2)
  }
}

val 異なる優先度の演算子が混在するテストのデータ = Seq(
  (
    "-a * b",
    "((-a) * b)"
  ),
  (
    "!-a",
    "(!(-a))"
  ),
  (
    "a + b+ c",
    "((a + b) + c)"
  ),
  (
    "a+b - c",
    "((a + b) - c)"
  ),
  (
    "a * b* c",
    "((a * b) * c)"
  ),
  (
    "a * b / c",
    "((a * b) / c)"
  ),
  (
    "a + b /c",
    "(a + (b / c))"
  ),
  (
    "a + b * c + d / e -f",
    "(((a + (b * c)) + (d / e)) - f)"
  ),
  (
    "5 > 4 == 3 < 4",
    "((5 > 4) == (3 < 4))"
  ),
  (
    "3 + 4 * 5 == 3 * 1 + 4 * 5",
    "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
  )
)

trait Node[T]:
  extension (t: T) def toStr: String

given Node[Program] with
  extension (p: Program) def toStr: String = p.map(_.toStr).mkString

given Node[Statement] with
  extension (t: Statement)
    def toStr: String = t match
      case Statement.LET(ident, expr) => s"let ${ident.showLiteral} = ${expr.toStr};"
      case Statement.RETURN(expr)     => s"return ${expr.toStr};"
      case Statement.EXPR(expr)       => expr.toStr

given Node[Expr] with
  extension (e: Expr)
    def toStr: String = e match
      case Expr.IDENT(ident)       => ident.showLiteral
      case Expr.INT(ident)         => ident.showLiteral
      case Expr.PREFIX(ident, r)   => s"(${ident.showLiteral}${r.toStr})"
      case Expr.INFIX(ident, l, r) => s"(${l.toStr} ${ident.showLiteral} ${r.toStr})"

extension (token: Token)
  def showLiteral: String = token match
    case Token.ILLEGAL              => 0.toChar.toString()
    case Token.EOF                  => 0.toChar.toString()
    case Token.ASSIGN               => "="
    case Token.PLUS                 => "+"
    case Token.MINUS                => "-"
    case Token.BANG                 => "!"
    case Token.ASTERISK             => "*"
    case Token.SLASH                => "/"
    case Token.LT                   => "<"
    case Token.GT                   => ">"
    case Token.EQ                   => "=="
    case Token.NotEQ                => "!="
    case Token.COMMA                => ","
    case Token.SEMICOLON            => ";"
    case Token.LPAREN               => "("
    case Token.RPAREN               => ")"
    case Token.LBRACE               => "{"
    case Token.RBRACE               => "}"
    case Token.FUNCTION             => "function"
    case Token.LET                  => "let"
    case Token.TRUE                 => "true"
    case Token.FALSE                => "false"
    case Token.IF                   => "if"
    case Token.ELSE                 => "else"
    case Token.RETURN               => "return"
    case Token.IDENT(value: String) => value
    case Token.INT(value: Int)      => value.toString()
