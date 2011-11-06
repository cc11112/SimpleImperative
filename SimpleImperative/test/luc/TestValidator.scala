package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import SimpleValidator._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class testValidator extends FunSuite {
  def testParse(description: String, v: Statement, b: Boolean) = {
    test(description) {
      assert(SimpleValidator.Check(v) == b)
    }
  }

  var s: Statement = new Assignment(Variable("n"), Constant(1))

  testParse("testcase1", s, true);

  s = new Assignment(Constant(1), Variable("n"))

  testParse("testcase2", s, false);

  s = new While(Variable("n"), s)

  testParse("testcase3", s, true);

  s = new While(Constant(1), s)

  testParse("testcase4", s, true);

  s = new While(Selection(Variable("n"), "next"), s)

  testParse("testcase5", s, true);

  s = new While(Assignment(Variable("n"), Constant(1)), s)

  testParse("testcase6", s, true);

  s = new While(While(Variable("n"), Constant(1)), s)

  testParse("testcase7", s, false);
  
  

}

