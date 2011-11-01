package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
    | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
    | term ~ "=" ~ term ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | term
    | factor)
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
    | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
    | factor ~ "While" ~ factor ^^ { case l ~ _ ~ r => While(l, r) }
    | factor)
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e  => Variable(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e })
  def selection: Parser[Any] = stringLiteral ~ "." ~ expr
  //  def obj: Parser[Map[String, Any]] = 
  //    "{" ~ repsep(member, ",") ~ "}" ^^ { case "{"~ ms ~"}" => Map() ++ ms }
}

