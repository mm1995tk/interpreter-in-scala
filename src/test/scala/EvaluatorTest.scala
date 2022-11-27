import token.Token.*
import lexer.Lexer
import parser.{parseProgram}
import env.Env
import evaluator.evalProgram
import obj.{getValue, Object, MonkeyPrimitiveType}
import cats.implicits.toShow
import ast.given

class EvaluatorTest extends munit.FunSuite {
  test("数値の評価") {
    val eitherResultOrErr = for {
      parsed <- parseProgram.runA("5")
      evaluated <- evalProgram(parsed).runA(Env())
    } yield evaluated

    eitherResultOrErr match
      case Right(v) => assertEquals(v, obj.Object.Int(5))
      case _        => assert(false)
  }

  test("真偽値の評価") {
    val eitherResultOrErr = for {
      parsed <- parseProgram.runA("true")
      evaluated <- evalProgram(parsed).runA(Env())
    } yield evaluated

    eitherResultOrErr match
      case Right(v) => assertEquals(v, obj.Object.Boolean(true))
      case _        => assert(false)
  }

  test("関数の評価") {
    val input = "let plus = fn (a,b) {return a + b;}; plus(1,2)"
    val eitherResultOrErr = for {
      parsed <- parseProgram.runA(input)
      evaluated <- evalProgram(parsed).runA(Env())
    } yield evaluated

    eitherResultOrErr match
      case Right(obj) => assertEquals(obj.unwrap.getValue, Some(3))
      case _          => assert(false)

  }

  test("高階関数の評価") {

    val input = "let a = 2; let b = 3;let newAdder = fn(x) {fn(y) {x+y}};let addT = newAdder(a); addT(b)"
    val eitherResultOrErr = for {
      parsed <- parseProgram.runA(input)
      evaluated <- evalProgram(parsed).runA(Env())
    } yield evaluated

    eitherResultOrErr match
      case Right(Object.Int(v)) => assertEquals(v, 5)
      case _                    => assert(false)

  }

  test("let文の評価") {
    val list: List[(String, scala.Int)] = List(
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a", 25),
      ("let a = 5; let b = a; b", 5),
      ("let a = 5; let b = a; let c= a+ b+ 5; c;", 15),
      ("let a = 3; let b = if (a > 1){ if (a > 2) {return a * 2;} return 4;};  b + 1 ", 7)
    )

    list.foreach((item) =>
      val (input, expected) = item

      val eitherResultOrErr = for {
        parsed <- parseProgram.runA(input)
        evaluated <- evalProgram(parsed).runA(Env())
      } yield evaluated

      eitherResultOrErr match
        case Right(Object.ReturnValue(v))  => assertEquals(v.getValue, Some(expected))
        case Right(v: MonkeyPrimitiveType) => assertEquals(v.getValue, Some(expected))
        case Left(e)                       => assert(false)
    )
  }
}
