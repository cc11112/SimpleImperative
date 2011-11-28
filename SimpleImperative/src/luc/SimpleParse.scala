package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    statement ~ "+" ~ statement ^^ { case l ~ _ ~ r => Plus(l, r) }
    | statement ~ "-" ~ statement ^^ { case l ~ _ ~ r => Minus(l, r) }
    | statement ~ "*" ~ statement ^^ { case l ~ _ ~ r => Times(l, r) }
    | statement ~ "/" ~ statement ^^ { case l ~ _ ~ r => Div(l, r) }
    | statement ~ "=" ~ expr ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | statement
    )
  def statement: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e => new Variable(e) }
    | "while" ~ expr ~ expr ^^ { case _ ~ l ~ r => While(l, r) }
//    | "new" ~ "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}"
//    	^^ { case _~ _ ~ e ~ _ ~ s ~ _ => New(GlobalStore.PutClass(e, Clazz(s: _*))) }
    | "new" ~ clazz ^^ { case _ ~ e => New(e)}
    | "new" ~ ident ^^ { case _ ~ e => New(GlobalStore.GetClass(e)) }
    | "(" ~> expr <~ ")" ^^ { case e => e }
    | "{" ~> repsep(expr, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
    | ident ~ "." ~ ident ^^ { case r ~ _ ~ f => Selection(Variable(r), f) }
    | ident ^^ { case s => Variable(s) })
  def clazz: Parser[Clazz] = (
    "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}" ^^ {
      case _ ~ e ~ _ ~ s ~ _ => GlobalStore.PutClass(e, Clazz(s: _*)) //insert to reference table
    })
  def value: Parser[Any] = opt(clazz) ~> ( expr | null )
  
}

