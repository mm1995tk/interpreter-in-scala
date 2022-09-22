import token.Token.*
import lexer.Lexer
class EvaluatorTest extends munit.FunSuite {
  test("数値の評価") {
    val lex = Lexer("5")
    parser.Parser(lex).parseProgram()._2.map(evaluator.Evaluator.apply) match
      case Right(Some(v)) => assertEquals(v, obj.Object.Int(5))
      case _              => assert(false)
  }

  test("真偽値の評価") {
    val lex = Lexer("true")
    parser.Parser(lex).parseProgram()._2.map(evaluator.Evaluator.apply) match
      case Right(Some(v)) => assertEquals(v, obj.Object.Boolean(true))
      case _              => assert(false)
  }
}
