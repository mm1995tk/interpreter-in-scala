package ast

opaque type Program = Seq[Statement]

enum Statement:
  case LET(ident: Ident, expr: Expr)
  case RETURN(expr: Expr)

enum Expr:
  case IDENT(ident: Ident)

opaque type Ident = String
