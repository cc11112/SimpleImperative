package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import TestFixtures._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class TestStatement extends FunSuite {

  def testValue(description: String, s: Statement, result: Int) = {
    test(description) {
      assert(SimpleImperative.apply(store)(varR).get === result)
    }
  }

  val store = Map[String, LValue[Int]](
    "x" -> Cell(2),
    "y" -> Cell(3),
    "r" -> Cell(0))

  var one: Statement = new Constant(1);
  var varX: Variable = new Variable("x");
  var varY: Variable = new Variable("y");
  var varR: Variable = new Variable("r");
  var s: Statement =
    new While(
      varY,
      new Sequence(
        new Assignment(varR, new Plus(varR, varX)),
        new Assignment(varY, new Minus(varY, one))));

  SimpleImperative.apply(store)(s)
  
  testValue("testcase1", s, 6)

}