package parser

import lexer.Lexer
import ast.*
import token.*

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838382+1;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("let x = 5;", "let y = 10;", "let foobar = (838382 + 1);")))
      assertEquals(stmt.toStr, expected)

  }

  test("let文解析エラーのテスト") {
    val input = "let x  5;\nlet  = 10;\nlet 838383;"
    val parser = Parser(Lexer(input))
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
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram()._2.getOrElse(Seq())

    if stmts.length != 3 then
      println(s"statementsの要素が3でない: ${stmts.length}")
      assert(false)

    for ((stmt, expected) <- stmts.zip(Seq("return 5;", "return ((15 + 5) - 10);", "return 838383;")))
      assertEquals(stmt.toStr, expected)

  }

  test("識別子のテスト") {
    val input = "foobar;"
    val parser = Parser(Lexer(input))
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
    val parser = Parser(Lexer(input))
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
    val parser = Parser(Lexer(input))
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
    val parser = Parser(Lexer(input))
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
      val parser = Parser(Lexer(test._1))
      val stmt = parser.parseProgram()._2.getOrElse(Seq()).head

      assertEquals(stmt.toStr, test._2)
  }

  test("if式のテスト") {
    val parser = Parser(Lexer("if (5 > 3) {let k = 2;return k + 1;} else {let k = 7;return k + 1;}"))
    val stmt = parser.parseProgram()._2.getOrElse(Seq()).head
    assertEquals(stmt.toStr, "if ((5 > 3)) {let k = 2;return (k + 1);} else {let k = 7;return (k + 1);}")
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

trait Node[T]:
  extension (t: T) def toStr: String

given Node[Program] with
  extension (p: Program) def toStr: String = p.map(_.toStr).mkString

given Node[Statement] with
  extension (t: Statement)
    def toStr: String = t match
      case Statement.Let(ident, expr) => s"let ${ident.showLiteral} = ${expr.toStr};"
      case Statement.Return(expr)     => s"return ${expr.toStr};"
      case Statement.Expr(expr)       => expr.toStr

given Node[Expr] with
  extension (e: Expr)
    def toStr: String = e match
      case Expr.Ident(ident)       => ident.showLiteral
      case Expr.Int(ident)         => ident.showLiteral
      case Expr.Prefix(ident, r)   => s"(${ident.showLiteral}${r.toStr})"
      case Expr.Infix(ident, l, r) => s"(${l.toStr} ${ident.showLiteral} ${r.toStr})"
      case Expr.Bool(token)        => token.equals(Token.True).toString()
      case Expr.If(cond, conseq, alter) =>
        s"if (${cond.toStr}) {${conseq.toStr}} ${alter match
            case Some(v) => s"else {${v.toStr}}"
            case None    => ""
          }"

extension (token: Token)
  def showLiteral: String = token match
    case Token.Illegal              => 0.toChar.toString()
    case Token.Eof                  => 0.toChar.toString()
    case Token.Assign               => "="
    case Token.Plus                 => "+"
    case Token.Minus                => "-"
    case Token.Bang                 => "!"
    case Token.Asterisk             => "*"
    case Token.Slash                => "/"
    case Token.Lt                   => "<"
    case Token.Gt                   => ">"
    case Token.Eq                   => "=="
    case Token.NotEq                => "!="
    case Token.Comma                => ","
    case Token.Semicolon            => ";"
    case Token.LeftParen            => "("
    case Token.RightParen           => ")"
    case Token.LeftBrace            => "{"
    case Token.RightBrace           => "}"
    case Token.Function             => "function"
    case Token.Let                  => "let"
    case Token.True                 => "true"
    case Token.False                => "false"
    case Token.If                   => "if"
    case Token.Else                 => "else"
    case Token.Return               => "return"
    case Token.Ident(value: String) => value
    case Token.Int(value: Int)      => value.toString()
