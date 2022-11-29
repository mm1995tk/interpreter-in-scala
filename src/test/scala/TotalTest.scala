import parser.{parseProgram}
import env.Env
import evaluator.evalProgram
import obj.{getValue, Object, MonkeyPrimitiveType}
import cats.implicits.toShow
import parser.ParserError
import evaluator.EvalError
import ast.given

class TotalTest extends munit.FunSuite {
  test("高階関数") {
    val source = scala.io.Source.fromResource("higher_order_function.monkey").getLines.mkString
    val env = Env()

    val eitherErrOrResult = for {
      program <- parseProgram.runA(source)
      result <- evalProgram(program).runA(env)
    } yield result

    eitherErrOrResult match
      case Left(value: ParserError) =>
        println("ParserError")
        println(value)
        assert(false)
      case Left(value: EvalError) =>
        println("EvalError")
        println(value)
        assert(false)
      case Right(value) =>
        assertEquals(value.unwrap.getValue, Some(9))
      case _ => assert(false)
  }

  test("フィボナッチ") {
    val source = scala.io.Source.fromResource("fibonacci.monkey").getLines.mkString
    val env = Env()

    val eitherErrOrResult = for {
      program <- parseProgram.runA(source)
      result <- evalProgram(program).runA(env)
    } yield result

    eitherErrOrResult match
      case Left(value: ParserError) =>
        println("ParserError")
        println(value)
        assert(false)
      case Left(value: EvalError) =>
        println("EvalError")
        println(value)
        assert(false)
      case Right(value) =>
        assertEquals(value.unwrap.getValue, Some(5))
      case _ => assert(false)
  }

  test("コラッツ予想") {
    val source = scala.io.Source.fromResource("collatz.monkey").getLines.mkString
    val env = Env()

    val eitherErrOrResult = for {
      program <- parseProgram.runA(source)
      _ = println(program.show)
      result <- evalProgram(program).runA(env)
    } yield result

    eitherErrOrResult match
      case Left(value: ParserError) =>
        println("ParserError")
        println(value)
        assert(false)
      case Left(value: EvalError) =>
        println("EvalError")
        println(value)
        assert(false)
      case Right(value) =>
        assertEquals(value.unwrap.getValue, Some(1))
      case _ => assert(false)
  }

  test("文は値を返さない") {
    val source = scala.io.Source.fromResource("stmt.monkey").getLines.mkString
    val env = Env()

    val eitherErrOrResult = for {
      program <- parseProgram.runA(source)
      result <- evalProgram(program).runA(env)
    } yield result

    eitherErrOrResult match
      case Left(value: ParserError) =>
        println("ParserError")
        println(value)
        assert(false)
      case Left(value: EvalError) =>
        println("EvalError")
        println(value)
        assert(false)
      case Right(value) =>
        assertEquals(value, Object.Null)
      case _ => assert(false)
  }
}
