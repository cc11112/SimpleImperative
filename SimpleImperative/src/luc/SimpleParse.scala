package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ expr ^^ { case l ~ _ ~ r => Plus(l, r) }
    | term ~ "-" ~ expr ^^ { case l ~ _ ~ r => Minus(l, r) }
    | term ~ "=" ~ expr ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | term
    | factor)
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
    | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
    | factor)
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e  => new Variable(e) }
    | "while" ~ expr ~ expr ^^ { case _ ~ l ~ r => While(l, r) }
  //  | "struct"  ~ obj  ^^ { case _ ~ e   => Clazz(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | "{" ~ expr ~ "}" ^^ { case _ ~ e ~ _  => Sequence(e) }
    | ident ^^ { case s  => Variable(s) }
    )
  def selection: Parser[Any] = stringLiteral ~ "." ~ expr
//  def obj: Parser[Any] = "{" ~ reqseq(ident, ",") ~ "}" 
//      "{" ~ repsep(selection, ",") ~ "}" ^^ { case "{" ~ ms ~ "}" => Map() ++ ms }
  //def member: Parser[Any] = stringLiteral ~ "." ~ expr
}

