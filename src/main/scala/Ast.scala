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

trait Node[T]:
  extension (t: T) def toStr: String

given Node[Program] with
  extension (p: Program) def toStr: String = p.map(_.toStr).mkString

given Node[Statement] with
  extension (t: Statement)
    def toStr: String = t match
      case Statement.Let(ident, expr) => s"let ${ident.showLiteral} = ${expr.toStr};"
      case Statement.Return(expr)     => s"return ${expr.toStr};"
      case Statement.Expr(expr)       => expr.toStr

given Node[Expr] with
  extension (e: Expr)
    def toStr: String = e match
      case Expr.Ident(ident)       => ident.showLiteral
      case Expr.Int(ident)         => ident.showLiteral
      case Expr.Prefix(ident, r)   => s"(${ident.showLiteral}${r.toStr})"
      case Expr.Infix(ident, l, r) => s"(${l.toStr} ${ident.showLiteral} ${r.toStr})"
      case Expr.Bool(token)        => token.equals(Token.True).toString()
      case Expr.Fn(params, body) =>
        s"${Token.Function.showLiteral}(${params.map(_._1.showLiteral).mkString(", ")}) {${body.toStr}}"
      case Expr.If(cond, conseq, alter) =>
        s"if (${cond.toStr}) {${conseq.toStr}} ${alter match
            case Some(v) => s"else {${v.toStr}}"
            case None    => ""
          }"
      case Expr.Call(fn, args) => s"${fn.toStr}(${args.map(_.toStr).mkString(", ")})"

extension (token: Token)
  def showLiteral: String = token match
    case Token.Illegal              => 0.toChar.toString()
    case Token.Eof                  => 0.toChar.toString()
    case Token.Assign               => "="
    case Token.Plus                 => "+"
    case Token.Minus                => "-"
    case Token.Bang                 => "!"
    case Token.Asterisk             => "*"
    case Token.Slash                => "/"
    case Token.Lt                   => "<"
    case Token.Gt                   => ">"
    case Token.Eq                   => "=="
    case Token.NotEq                => "!="
    case Token.Comma                => ","
    case Token.Semicolon            => ";"
    case Token.LeftParen            => "("
    case Token.RightParen           => ")"
    case Token.LeftBrace            => "{"
    case Token.RightBrace           => "}"
    case Token.Function             => "fn"
    case Token.Let                  => "let"
    case Token.True                 => "true"
    case Token.False                => "false"
    case Token.If                   => "if"
    case Token.Else                 => "else"
    case Token.Return               => "return"
    case Token.Ident(value: String) => value
    case Token.Int(value: Int)      => value.toString()
