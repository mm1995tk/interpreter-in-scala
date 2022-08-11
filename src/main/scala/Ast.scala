package ast

type Program = Seq[Statement]

enum Statement:
  case LET(ident: Ident, expr: Expr)
  case RETURN(expr: Expr)

enum Expr:
  case IDENT(ident: Ident)

opaque type Ident = String
object Ident:
  def apply(str: String): Ident = str
