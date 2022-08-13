import lexer.Lexer
import parser.Parser
import ast.Statement
import token.Token
import parser.ParserError

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

}

enum LetTestErr:
  case NotLetStatement, NotMatchName

def contentOfTestLetStatements(statement: Statement, name: String): Option[LetTestErr] =
  statement match
    case Statement.LET(ident, _) => {
      if ident.toString == name then None else Some(LetTestErr.NotMatchName)
    }
    case _ => Some(LetTestErr.NotLetStatement)
