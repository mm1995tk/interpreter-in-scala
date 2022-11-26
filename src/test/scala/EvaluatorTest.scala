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

  // test("関数の評価") {
  //   val input = "let plus = fn (a,b) {return a + b;}; plus(1,2)"
  //   val (_, parsed) = Parser(input).parseProgram()
  //   val env = Env()
  //   parsed match
  //     case Left(e) =>
  //       println(e)
  //       assert(false)
  //     case Right(value) =>
  //       evalProgram(value).runA(env).map(_.unwrap) match
  //         case Right(Object.Int(v)) =>
  //           assertEquals(v, 3)
  //         case Left(e) =>
  //           // println(e)
  //           assert(false)
  //         case _ =>
  //           assert(false)

  // }

  // test("高階関数の評価") {

  //   val input = "let a = 2; let b = 3;let newAdder = fn(x) {fn(y) {x+y}};let addT = newAdder(a);addT(b)"
  //   val (_, parsed) = Parser(input).parseProgram()
  //   val env = Env()
  //   parsed match
  //     case Left(e) =>
  //       println(e)
  //       assert(false)
  //     case Right(value) =>
  //       evalProgram(value).runA(env) match
  //         case Right(Object.Int(v)) =>
  //           assertEquals(v, 5)
  //         case Left(e) =>
  //           // println(e)
  //           assert(false)
  //         case _ =>
  //           assert(false)

  // }

  // test("let文の評価") {
  //   val list: List[(String, scala.Int)] = List(
  //     ("let a = 5; a;", 5),
  //     ("let a = 5 * 5; a;", 25),
  //     ("let a = 5; let b = a; b;", 5),
  //     ("let a = 5; let b = a; let c= a+ b+ 5; c;", 15),
  //     ("let a = 3; let b = if (a > 1){ if (a > 2) {return a * 2;} return 4;}  b + 1 ", 7)
  //   )

  //   list.foreach((item) =>
  //     val (input, expected) = item
  //     val (_, parsed) = Parser(input).parseProgram()
  //     val env = Env()
  //     parsed match
  //       case Left(e) =>
  //         println(e)
  //         assert(false)
  //       case Right(value) =>
  //         evalProgram(value).runA(env) match
  //           case Right(v) =>
  //             v match
  //               case Object.ReturnValue(v)  => assertEquals(v.getValue, Some(expected))
  //               case v: MonkeyPrimitiveType => assertEquals(v.getValue, Some(expected))

  //           case Left(e) =>
  //             println(e)

  //             assert(false)
  //   )
  // }
}
