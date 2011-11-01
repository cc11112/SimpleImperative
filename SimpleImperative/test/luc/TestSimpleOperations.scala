package cs.luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import ExprParser._

@RunWith(classOf[JUnitRunner])
class TestSimpleOperations extends FunSuite {
  def testValue(description: String, v: Expr, s: String) = {
    test(description) {
      val parsedExpr = ExprParser.parseAll(ExprParser.expr, s)
      assert(parsedExpr.get === v)
    }
  }

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)),
        Times(
          Constant(3),
          Constant(4))),
      Constant(5));

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  testValue("test1", complex1, complex1string);

}