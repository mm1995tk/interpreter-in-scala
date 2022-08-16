package parser

import lexer.Lexer
import ast.*
import token.Token

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, ident) <- stmts.zip(Seq("x", "y", "foobar")))
      assert {
        contentOfTestLetStatements(stmt, ident) match
          case None => true
          case Some(err) =>
            println(err)
            false
      }

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

    val expecteds = Seq((Token.MINUS, 15), (Token.BANG, 5))

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
    val expecteds =
      Seq(Token.PLUS, Token.MINUS, Token.ASTERISK, Token.SLASH, Token.GT, Token.LT, Token.EQ, Token.NotEQ)

    stmts.zip(expecteds).foreach { (stmt, expected) =>
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

      assertEquals(stmt, Statement.EXPR(test._2))
  }
}

enum LetTestErr:
  case NotLetStatement, NotMatchName

def contentOfTestLetStatements(statement: Statement, name: String): Option[LetTestErr] =
  statement match
    case Statement.LET(Token.IDENT(value), _) => {
      if value == name then None else Some(LetTestErr.NotMatchName)
    }
    case _ => Some(LetTestErr.NotLetStatement)

val 異なる優先度の演算子が混在するテストのデータ = Seq(
  (
    "-a * b",
    Expr.INFIX(
      Token.ASTERISK,
      Expr.PREFIX(Token.MINUS, Expr.IDENT(Token.IDENT("a"))),
      Expr.IDENT(Token.IDENT("b"))
    )
  ),
  (
    "!-a",
    Expr.PREFIX(
      Token.BANG,
      Expr.PREFIX(Token.MINUS, Expr.IDENT(Token.IDENT("a")))
    )
  ),
  (
    "a + b+ c",
    Expr.INFIX(
      Token.PLUS,
      Expr.INFIX(
        Token.PLUS,
        Expr.IDENT(Token.IDENT("a")),
        Expr.IDENT(Token.IDENT("b"))
      ),
      Expr.IDENT(Token.IDENT("c"))
    )
  ),
  (
    "a+b - c",
    Expr.INFIX(
      Token.MINUS,
      Expr.INFIX(
        Token.PLUS,
        Expr.IDENT(Token.IDENT("a")),
        Expr.IDENT(Token.IDENT("b"))
      ),
      Expr.IDENT(Token.IDENT("c"))
    )
  ),
  (
    "a * b* c",
    Expr.INFIX(
      Token.ASTERISK,
      Expr.INFIX(
        Token.ASTERISK,
        Expr.IDENT(Token.IDENT("a")),
        Expr.IDENT(Token.IDENT("b"))
      ),
      Expr.IDENT(Token.IDENT("c"))
    )
  ),
  (
    "a * b / c",
    Expr.INFIX(
      Token.SLASH,
      Expr.INFIX(
        Token.ASTERISK,
        Expr.IDENT(Token.IDENT("a")),
        Expr.IDENT(Token.IDENT("b"))
      ),
      Expr.IDENT(Token.IDENT("c"))
    )
  ),
  (
    "a + b /c",
    Expr.INFIX(
      Token.PLUS,
      Expr.IDENT(Token.IDENT("a")),
      Expr.INFIX(
        Token.SLASH,
        Expr.IDENT(Token.IDENT("b")),
        Expr.IDENT(Token.IDENT("c"))
      )
    )
  ),
  (
    "a + b * c + d / e -f",
    Expr.INFIX(
      Token.MINUS,
      Expr.INFIX(
        Token.PLUS,
        Expr.INFIX(
          Token.PLUS,
          Expr.IDENT(Token.IDENT("a")),
          Expr.INFIX(
            Token.ASTERISK,
            Expr.IDENT(Token.IDENT("b")),
            Expr.IDENT(Token.IDENT("c"))
          )
        ),
        Expr.INFIX(
          Token.SLASH,
          Expr.IDENT(Token.IDENT("d")),
          Expr.IDENT(Token.IDENT("e"))
        )
      ),
      Expr.IDENT(Token.IDENT("f"))
    )
  ),
  (
    "5 > 4 == 3 < 4",
    Expr.INFIX(
      Token.EQ,
      Expr.INFIX(Token.GT, Expr.INT(Token.INT(5)), Expr.INT(Token.INT(4))),
      Expr.INFIX(Token.LT, Expr.INT(Token.INT(3)), Expr.INT(Token.INT(4)))
    )
  ),
  (
    "3 + 4 * 5 == 3 * 1 + 4 * 5",
    Expr.INFIX(
      Token.EQ,
      Expr.INFIX(
        Token.PLUS,
        Expr.INT(Token.INT(3)),
        Expr.INFIX(
          Token.ASTERISK,
          Expr.INT(Token.INT(4)),
          Expr.INT(Token.INT(5))
        )
      ),
      Expr.INFIX(
        Token.PLUS,
        Expr.INFIX(
          Token.ASTERISK,
          Expr.INT(Token.INT(3)),
          Expr.INT(Token.INT(1))
        ),
        Expr.INFIX(
          Token.ASTERISK,
          Expr.INT(Token.INT(4)),
          Expr.INT(Token.INT(5))
        )
      )
    )
  )
)

// 以下、テストを楽にする仕込み

trait Node[T]:
  extension (t: T) def toStr: String

given Node[Program] with
  extension (p: Program) def toStr: String = p.map(_.toStr).mkString

given Node[Statement] with
  extension (t: Statement)
    def toStr: String = t match
      case Statement.LET(ident, expr) => ???
      case Statement.RETURN(expr)     => ???
      case Statement.EXPR(expr)       => ???

given Node[Expr] with
  extension (e: Expr)
    def toStr: String = e match
      case Expr.IDENT(ident)       => ???
      case Expr.INT(ident)         => ???
      case Expr.PREFIX(ident, r)   => ???
      case Expr.INFIX(ident, r, l) => ???
