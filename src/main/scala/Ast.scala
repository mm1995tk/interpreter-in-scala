package ast

import token.*

type Program = Seq[Statement]

enum Statement:
  case Let(ident: Token.Ident, expr: ast.Expr)
  case Return(expr: ast.Expr)
  case Expr(expr: ast.Expr)

enum Expr:
  case Ident(token: Token.Ident)
  case Int(token: Token.Int)
  case Prefix(token: PrefixToken, right: Expr)
  case Infix(token: InfixToken, left: Expr, right: Expr)
  case Bool(token: BoolToken)
  case If(cond: Expr, consequence: Program, alter: Option[Program])
  case Fn(parms: Seq[Expr.Ident], body: Program)
  case Call(fn: Expr.Ident | Expr.Fn, args: Seq[Expr])
