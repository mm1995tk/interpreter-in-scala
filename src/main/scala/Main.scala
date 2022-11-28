import parser.{parseProgram, ParserError, given}
import evaluator.{evalProgram, EvalError, given}
import obj.{Object, given}
import env.Env
import cats.implicits.toShow

@main def main: Unit =
  println("\nWelcome to Monkey Language!\n");

  repl(Some(Env()))

def repl(env: Option[Env]): Option[Env] = env match
  case None => None
  case Some(env) =>
    print("\n>> ")

    val input = scala.io.StdIn.readLine()
    if (input == ":q") {
      println("\nthanks for using!\n");
      return None
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

    repl(Some(nextEnv))
