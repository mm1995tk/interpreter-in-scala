package parser

import lexer.Lexer
import ast.{given, *}
import token.*
import cats.implicits.toShow

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838382+1;"
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("let x = 5;", "let y = 10;", "let foobar = (838382 + 1);")))
      assertEquals(stmt.show, expected)

  }

  test("前置演算子のテスト") {
    val input = "-15;!5;"
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

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
  test("前置演算子のテスト2") {
    val input = "-15"
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val expecteds: Seq[(PrefixToken, Int)] = Seq((Token.Minus, 15))

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
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

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
      val parsed = parseProgram.runA(test._1)
      val stmt = parsed.getOrElse(Seq()).head

      assertEquals(stmt.show, test._2)
  }

  test("if式のテスト") {
    val parsed = parseProgram.runA("if (5 > 3) {let k = 2;return k + 1;} else {let k = 7;return k + 1;}")
    parsed.getOrElse(Seq()).headOption match
      case Some(stmt) =>
        assertEquals(stmt.show, "if ((5 > 3)) {let k = 2;return (k + 1);} else {let k = 7;return (k + 1);}")
      case None => assert(false)
  }

  // test("let文解析エラーのテスト") {
  //   val input = "let x  5;\nlet  = 10;\nlet 838383;"
  //   val parser = Parser(input)
  //   val stmts = parser.parseProgram()._2

  //   stmts match
  //     case Right(_) => assert(false)
  //     case Left(err) =>
  //       assertEquals(
  //         err,
  //         ParserError.UnexpectedToken(Token.Int(5), Token.Assign)
  //       )

  // }

  test("return文のテスト") {
    val input = "return  5;\nreturn 15+5-10;\nreturn 838383;"
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("return 5;", "return ((15 + 5) - 10);", "return 838383;")))
      assertEquals(stmt.show, expected)

  }

  test("識別子のテスト") {
    val input = "foobar;"
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

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
    val parsed = parseProgram.runA(input)
    val stmts = parsed.getOrElse(Seq())

    if stmts.length != 1 then
      println(s"statementsの要素が1でない: ${stmts.length}")
      assert(false)

    val stmt = stmts.head
    assert(stmt match
      case Statement.Expr(Expr.Int(Token.Int(value))) => value == 5
      case _                                          => false
    )

  }

  test("関数リテラルのテスト") {
    val parsed = parseProgram.runA("let k = fn(x, y) {x+y};")
    parsed match
      case Right(v) => assertEquals(v.show, "let k = fn(x, y) {(x + y)};")
      case Left(v) =>
        println(v)
        assert(false)
  }

  test("改行を含む関数リテラルのテスト") {
    val parsed = parseProgram.runA("let k = fn(x, y) {let v = x*2;\nv+y};")
    parsed match
      case Right(v) => assertEquals(v.show, "let k = fn(x, y) {let v = (x * 2);(v + y)};")
      case Left(v) =>
        println(v)
        assert(false)
  }

  // test("関数呼び出しのテスト") {
  //   val parser = Parser(("add(1 +4, 2)"))
  //   parser.parseProgram()._2 match
  //     case Right(v) => assertEquals(v.toStr, "add((1 + 4), 2)")
  //     case Left(v) =>
  //       println(v)
  //       assert(false)

  // }

  // test("関数呼び出しのテスト(即時実行)") {
  //   val parser = Parser(("fn(x, y) {x+ y}(1 +4, 2)"))
  //   parser.parseProgram()._2 match
  //     case Right(v) => assertEquals(v.toStr, "fn(x, y) {(x + y)}((1 + 4), 2)")
  //     case Left(v) =>
  //       println(v)
  //       assert(false)

  // }
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
