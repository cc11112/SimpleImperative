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

  testParse("test1", complex1, complex1string);

  //test case 1
  val store = Map[String, Cell]("r" -> Cell(0))
  var varR: Variable = new Variable("r");
  var s: Statement = new Assignment(varR, complex1)

  SimpleImperative.apply(store)(s)

  testValue("testcase1", SimpleImperative.getCell(store, varR), -3)
}