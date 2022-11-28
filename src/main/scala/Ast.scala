package ast

import token.{*, given}
import cats.Show
import cats.given
import cats.implicits.*

type Program = Seq[Statement]

enum Statement:
  case Let(ident: Token.Ident, expr: ast.Expr)
  case Return(expr: ast.Expr)
  case Expr(expr: ast.Expr)

enum Expr:
  case Null
  case Ident(token: Token.Ident)
  case Int(token: Token.Int)
  case Prefix(token: PrefixToken, right: Expr)
  case Infix(token: InfixToken, left: Expr, right: Expr)
  case Bool(token: BoolToken)
  case If(cond: Expr, consequence: Program, alter: Option[Program])
  case Fn(parms: Seq[Expr.Ident], body: Program)
  case Call(fn: Expr.Ident | Expr.Fn, args: Seq[Expr])

given Show[Program] with
  def show(program: Program): String = program.map(_.show).mkString

given Show[Statement] with
  def show(expr: Statement): String = expr match
    case Statement.Let(ident, expr) => s"let ${ident.asInstanceOf[Token].show} = ${expr.show};"
    case Statement.Return(expr)     => s"return ${expr.show};"
    case Statement.Expr(expr)       => expr.show

given Show[Expr] with
  def show(t: Expr): String = t match
    case Expr.Null               => "null"
    case Expr.Ident(ident)       => ident.asInstanceOf[Token].show
    case Expr.Int(ident)         => ident.asInstanceOf[Token].show
    case Expr.Prefix(ident, r)   => s"(${ident.asInstanceOf[Token].show}${r.show})"
    case Expr.Infix(ident, l, r) => s"(${l.show} ${ident.asInstanceOf[Token].show} ${r.show})"
    case Expr.Bool(token)        => token.equals(Token.True).toString()
    case Expr.Fn(params, body) =>
      s"${Token.Function.show}(${params.map(_._1.asInstanceOf[Token].show).mkString(", ")}) {${body.show}}"
    case Expr.If(cond, conseq, alter) =>
      s"if (${cond.show}) {${conseq.show}} ${alter match
          case Some(v) => s"else {${v.show}}"
          case None    => ""
        }"
    case Expr.Call(fn, args) => s"${fn.asInstanceOf[Expr].show}(${args.map(_.show).mkString(", ")})"
