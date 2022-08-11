import lexer.Lexer
import parser.Parser
import ast.Statement
class ParserTest extends munit.FunSuite {

  test("let文のテスト") {

    val input = f"let x = 5;\nlet y = 10;\nlet foobar = 838383;"
    val parser = Parser(Lexer(input))
    val stmts = parser.parseProgram

    if stmts.length != 3 then {
      println(s"statementsの要素が3でない: ${stmts.length}")
      assertEquals(true, false)
    }

    for ((stmt, ident) <- stmts.zip(Seq("x", "y", "foobar"))) {

      contentOfTestLetStatements(stmt, ident) match
        case None => assertEquals(true, true)
        case Some(err) =>
          println(err)
    }

  }
  // test("return文のテスト") {}

}

enum LetTestErr:
  case NotLetStatement, NotMatchName

def contentOfTestLetStatements(statement: Statement, name: String): Option[LetTestErr] =
  statement match
    case Statement.LET(ident, _) => {
      if ident.toString == name then None else Some(LetTestErr.NotMatchName)
    }
    case _ => Some(LetTestErr.NotLetStatement)
