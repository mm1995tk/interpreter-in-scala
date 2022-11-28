import parser.{parseProgram}
import env.Env
import evaluator.evalProgram
import obj.{getValue, Object, MonkeyPrimitiveType}
import cats.implicits.toShow
import parser.ParserError
import evaluator.EvalError

class TotalTest extends munit.FunSuite {

  test("ソースコード") {
    val source = scala.io.Source.fromResource("test.monkey").getLines.mkString
    val env = Env()

    val eitherErrOrResult = for {
      program <- parseProgram.runA(source)
      result <- evalProgram(program).runA(env)
    } yield result

    eitherErrOrResult match
      case Left(value) =>
        print(value)
        assert(false)
      case Right(value) =>
        assertEquals(value.unwrap.getValue, Some(9))

  }

}
