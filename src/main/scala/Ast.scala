package ast

import token.Token

type Program = Seq[Statement]

enum Statement:
  case LET(ident: Ident, expr: Expr)
  case RETURN(expr: Expr)
  case EXPR(expr: Expr)

enum Expr:
  case IDENT(ident: Ident)

private type Ident = Token.IDENT
