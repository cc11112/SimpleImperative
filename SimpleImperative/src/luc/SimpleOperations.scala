package cs.luc

import scala.util.parsing.combinator._

/**
 * An initial algebra of arithmetic expressions.
 */
abstract class Expr
abstract class CompositeExpr(val left: Expr, val right: Expr) extends Expr {
  if (left == null || right == null) {
    throw new IllegalArgumentException("null subexpression")
  }
}

case class Constant(value: Int) extends Expr
case class Plus(override val left: Expr, override val right: Expr) extends CompositeExpr(left, right)
case class Minus(override val left: Expr, override val right: Expr) extends CompositeExpr(left, right)
case class Times(override val left: Expr, override val right: Expr) extends CompositeExpr(left, right)
case class Div(override val left: Expr, override val right: Expr) extends CompositeExpr(left, right)

object ExprParser extends JavaTokenParsers {
  def expr: Parser[Expr] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
  | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
  | term
  | factor
  )
  def term: Parser[Expr] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
  | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
  | factor
  )
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
  | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e } 
  )
}
