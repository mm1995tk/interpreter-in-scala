package parser

import lexer.Lexer
import ast.{given, *}
import token.*

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838382+1;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("let x = 5;", "let y = 10;", "let foobar = (838382 + 1);")))
      assertEquals(stmt.toStr, expected)

  }

  test("let文解析エラーのテスト") {
    val input = "let x  5;\nlet  = 10;\nlet 838383;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2

    stmts match
      case Right(_) => assert(false)
      case Left(seq) =>
        assertEquals(
          seq.lift.filterNot(_.equals(ParserError.NotImplemented)),
          List(
            ParserError.UnexpectedToken(Token.Int(5), Token.Assign),
            ParserError.UnexpectedToken(
              Token.Assign,
              Token.Ident("variable names")
            ),
            ParserError.UnexpectedToken(
              Token.Int(838383),
              Token.Ident("variable names")
            )
          )
        )

  }

  test("return文のテスト") {
    val input = "return  5;\nreturn 15+5-10;\nreturn 838383;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("return 5;", "return ((15 + 5) - 10);", "return 838383;")))
      assertEquals(stmt.toStr, expected)

  }

  test("識別子のテスト") {
    val input = "foobar;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val stmt = stmts.head
    assert(stmt match
      case Statement.Expr(Expr.Ident(Token.Ident(value))) => value == "foobar"
      case _                                              => false
    )

  }

  test("整数リテラルのテスト") {
    val input = "5;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val stmt = stmts.head
    assert(stmt match
      case Statement.Expr(Expr.Int(Token.Int(value))) => value == 5
      case _                                          => false
    )

  }

  test("前置演算子のテスト") {
    val input = "-15;!5;"
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 2 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val expecteds: Seq[(PrefixToken, Int)] = Seq((Token.Minus, 15), (Token.Bang, 5))

    stmts.zip(expecteds).foreach { (stmt, expected) =>
      assertEquals(
        stmt,
        Statement.Expr {
          Expr.Prefix(expected._1, Expr.Int(Token.Int(expected._2)))
        }
      )
    }

  }

  test("中置演算子のテスト") {
    val input =
      Seq("5 + 5;", "5 - 5;", "5 * 5;", "5 / 5;", "5 > 5;", "5 < 5;", "5 == 5;", "5 != 5;").mkString
    val parser = Parser(input)
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 8 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    assert(true)
    val expecteds: Seq[InfixToken] =
      Seq(Token.Plus, Token.Minus, Token.Asterisk, Token.Slash, Token.Gt, Token.Lt, Token.Eq, Token.NotEq)

    val iter: Seq[(Statement, InfixToken)] = stmts.zip(expecteds)

    iter.foreach { item =>
      val stmt = item._1
      val expected: InfixToken = item._2
      assertEquals(
        stmt,
        Statement.Expr {
          Expr.Infix(expected, Expr.Int(Token.Int(5)), Expr.Int(Token.Int(5)))
        }
      )
    }

  }

  test("異なる優先度の演算子が混在するテスト") {
    for (test <- 異なる優先度の演算子が混在するテストのデータ)
      val parser = Parser((test._1))
      val stmt = parser.parseProgram()._2.getOrElse(Seq()).head

      assertEquals(stmt.toStr, test._2)
  }

  test("if式のテスト") {
    val parser = Parser(("if (5 > 3) {let k = 2;return k + 1;} else {let k = 7;return k + 1;}"))
    val stmt = parser.parseProgram()._2.getOrElse(Seq()).head
    assertEquals(stmt.toStr, "if ((5 > 3)) {let k = 2;return (k + 1);} else {let k = 7;return (k + 1);}")
  }

  test("関数リテラルのテスト") {
    val parser = Parser(("let k = fn(x, y) {x+y}"))
    parser.parseProgram()._2 match
      case Right(v) => assertEquals(v.toStr, "let k = fn(x, y) {(x + y)};")
      case Left(v) =>
        println(v)
        assert(false)
  }

  test("改行を含む関数リテラルのテスト") {
    val parser = Parser(("let k = fn(x, y) {let v = x*2;\nv+y}"))
    parser.parseProgram()._2 match
      case Right(v) => assertEquals(v.toStr, "let k = fn(x, y) {let v = (x * 2);(v + y)};")
      case Left(v) =>
        println(v)
        assert(false)
  }

  test("関数呼び出しのテスト") {
    val parser = Parser(("add(1 +4, 2)"))
    parser.parseProgram()._2 match
      case Right(v) => assertEquals(v.toStr, "add((1 + 4), 2)")
      case Left(v) =>
        println(v)
        assert(false)

  }

  test("関数呼び出しのテスト(即時実行)") {
    val parser = Parser(("fn(x, y) {x+ y}(1 +4, 2)"))
    parser.parseProgram()._2 match
      case Right(v) => assertEquals(v.toStr, "fn(x, y) {(x + y)}((1 + 4), 2)")
      case Left(v) =>
        println(v)
        assert(false)

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
    "5 > 4 == true",
    "((5 > 4) == true)"
  ),
  (
    "3 + 4 * 5 == 3 * 1 + 4 * 5",
    "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
  ),
  (
    "false == 3 * 1 + 4 * 5",
    "(false == ((3 * 1) + (4 * 5)))"
  ),
  ("!true", "(!true)"),
  ("(5+5) * 7", "((5 + 5) * 7)")
)
