package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import StatementParser._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class TestSimpleParse extends FunSuite {
  def testParse(description: String, v: Statement, s: String) = {
    test(description) {
      val parsedExpr = StatementParser.parseAll(StatementParser.expr, s)
      assert(parsedExpr.get === v)
    }
  }

  def testValue(description: String, cell: Cell, result: Int) = {
    test(description) {
      assert(SimpleImperative.evaluate(cell) === result)
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
      Constant(3));

  val complex1string = "((1 + 2) - (3 * 4)) / 3"

  testParse("testcase1", complex1, complex1string);

  //test case 1
  val store = Map[String, Cell](
    "r" -> Cell(0),
    "q" -> Cell(0),
    "s" -> Cell(0)
    )

  var varR: Variable = new Variable("r");
  var s: Statement = new Assignment(varR, complex1)

  SimpleImperative.apply(store)(s)

  testValue("testcase2", SimpleImperative.getCell(store, varR), -3)

  val exp2 = new Assignment(new Variable("q"),  Constant(5))
  val complex1string2 = "var q = 5"

  testParse("testcase3", exp2, complex1string2);
  var varS: Variable = new Variable("s");
  SimpleImperative.apply(store)(new Assignment(varS, exp2))
  testValue("testcase4",  SimpleImperative.getCell(store, varS), 5)
  
}