import lexer.Lexer
import parser.Parser
import ast.Statement
import token.Token

class ParserTest extends munit.FunSuite {

  test("let文のテスト") {
    val input = "let x = 5;\nlet y = 10;\nlet foobar = 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram

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

  test("return文のテスト") {
    val input = "return  5;\nreturn 10;\nreturn 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram

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
