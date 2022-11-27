import scala.sys.process.processInternal
import parser.{Parser, ParserError, parseProgram, EitherParserErrorOr, given}
import obj.{Object, given}
import evaluator.{evalProgram, EvalError, EitherEvalErrorOr, given}
import env.Env
import cats.implicits.toShow

@main def main: Unit =
  println("\nWelcome to Monkey Language!");
  println("");
  val env = Env()

  repl(env)

def repl(env: Env): Env =
  print(">> ")
  val input = scala.io.StdIn.readLine()
  if (input == ":q") {
    println("");
    println("thanks for using!");
    println("");

    System.exit(0);
  }
  println("");

  val eitherErrOrResult: Either[ParserError | EvalError, (Env, Object)] = for {
    program <- parseProgram.runA(input)
    result <- evalProgram(program).run(env)
  } yield result

  val (nextEnv, result) = eitherErrOrResult match
    case Left(err: ParserError) => env -> err.show
    case Left(err: EvalError)   => env -> err.show
    case Right((e, obj))        => e -> obj.show

  println(result)

  repl(nextEnv)
