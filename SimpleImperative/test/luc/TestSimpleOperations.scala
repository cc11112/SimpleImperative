package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import SimpleOperations._
import TestFixtures._

@RunWith(classOf[JUnitRunner])
class TestSimpleOperations extends FunSuite {
  def testValue(description: String, v: AbstractStatement, result: Int) = {
    test(description) {
      assert(evaluate(v) === result)
    }
  }

  testValue("const", new Constant(1), 1);
  
  testValue("plus", new Plus(new Constant(1), new Constant(2)) , 3);
  
  testValue("min",  new Minus(new Constant(6), new Constant(4)),  2);
  
}