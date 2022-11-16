import token.Token.*
import lexer.Lexer
import parser.Parser
import env.Env
import evaluator.evalProgram
import obj.{getValue, Object, MonkeyPrimitiveType}

class EvaluatorTest extends munit.FunSuite {
  // test("数値の評価") {
  //   val lex = Lexer("5")
  //   evaluator.Evaluator(parser.Parser(lex).parseProgram()._2) match
  //     case Right(v) => assertEquals(v, obj.Object.Int(5))
  //     case _        => assert(false)
  // }

  // test("真偽値の評価") {
  //   val lex = Lexer("true")
  //   evaluator.Evaluator(parser.Parser(lex).parseProgram()._2) match
  //     case Right(v) => assertEquals(v, obj.Object.Boolean(true))
  //     case _        => assert(false)
  // }

  test("let文の評価") {
    val list: List[(String, scala.Int)] = List(
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c= a+ b+ 5; c;", 15),
      ("let a = 3; let b = if (a > 1){ if (a > 2) {return a * 2;} return 4;}  b + 1 ", 7)
    )

    list.foreach((item) =>
      val (input, expected) = item
      val (_, parsed) = Parser(Lexer(input)).parseProgram()
      val env = Env()
      parsed match
        case Left(e) =>
          println(e)
          assert(false)
        case Right(value) =>
          val (_, result) = evalProgram(value, env)
          result match
            case Right(v) =>
              v match
                case Object.ReturnValue(v)  => assertEquals(v.getValue, Some(expected))
                case v: MonkeyPrimitiveType => assertEquals(v.getValue, Some(expected))

            case Left(e) =>
              println(e)

              assert(false)
    )
  }
}
