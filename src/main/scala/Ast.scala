package ast

import token.*

type Program = Seq[Statement]

enum Statement:
  case LET(ident: Ident, expr: Expr)
  case RETURN(expr: Expr)
  case EXPR(expr: Expr)

enum Expr:
  case IDENT(ident: Ident)
  case INT(ident: Token.INT)
  case PREFIX(ident: PrefixToken, right: Expr)
  case INFIX(ident: InfixToken, left: Expr, right: Expr)

private type Ident = Token.IDENT
