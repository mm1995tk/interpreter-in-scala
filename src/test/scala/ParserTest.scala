package parser

import lexer.Lexer
import ast.Statement
import token.Token
import ast.Expr

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
}

enum LetTestErr:
  case NotLetStatement, NotMatchName

def contentOfTestLetStatements(statement: Statement, name: String): Option[LetTestErr] =
  statement match
    case Statement.LET(Token.IDENT(value), _) => {
      if value == name then None else Some(LetTestErr.NotMatchName)
    }
    case _ => Some(LetTestErr.NotLetStatement)
