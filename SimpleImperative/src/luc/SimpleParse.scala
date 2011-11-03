package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ expr ^^ { case l ~ _ ~ r => Plus(l, r) }
    | statement ~ "-" ~ statement ^^ { case l ~ _ ~ r => Minus(l, r) }
    | term ~ "=" ~ expr ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | term
    | statement)
  def term: Parser[Statement] = (
    statement ~ "*" ~ statement ^^ { case l ~ _ ~ r => Times(l, r) }
    | statement ~ "/" ~ statement ^^ { case l ~ _ ~ r => Div(l, r) }
    | statement)
  def statement: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e => new Variable(e) }
    | "while" ~ expr ~ expr ^^ { case _ ~ l ~ r => While(l, r) }
    | "struct" ~ clazz ^^ { case _ ~ e => New(e) }
    | "(" ~> expr <~ ")" ^^ { case e => e }
    | "{" ~> repsep(expr, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
    | ident ^^ { case s => Variable(s) })
  def clazz: Parser[Clazz] = (
    ident ~ "{" ~ repsep(ident, ",") ~ "}" ^^ { case e ~ _ ~ s ~ _ => new Clazz(s: _*) })
  def value: Parser[Any] = obj
  def arr: Parser[Seq[String]] = "(" ~> repsep(stringLiteral, ",") <~ ")"
  def member: Parser[Any] = stringLiteral | stringLiteral ~ "." ~ value
  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

}

