import scala.sys.process.processInternal
import lexer.Lexer
import token.Token
import parser.{Parser, ParserError, parseProgram, EitherParserErrorOr, given}
import ast.given
import obj.Object
import evaluator.{evalProgram, EvalError, EitherEvalErrorOr, given}
import env.Env
import obj.getValue
import cats.data.StateT
import cats.implicits._
import cats.Show

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

given Show[Object] with
  def show(obj: Object) = obj match
    case Object.Int(value)         => value.toString()
    case Object.Boolean(value)     => value.toString()
    case Object.ReturnValue(value) => value.getValue.getOrElse("null").toString()
    case Object.Function(_, _, _)  => "function"
    case Object.Null               => "null"
