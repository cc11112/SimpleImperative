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

  //var n = 1 --> OK
  testParse("testcase1", Assignment(Variable("n"), Constant(1)), true);

  // 1 = n -> error
  testParse("testcase2", Assignment(Constant(1), Variable("n")), false);

  //while(n) {n = 1} --> OK
  testParse("testcase3", While(Variable("n"), Assignment(Variable("n"), Constant(1))), true);

  //while(n=1) {n = 2} -->OK
  testParse("testcase4", While(Assignment(Variable("n"), Constant(1)), Assignment(Variable("n"), Constant(2))), true);

  //while(1) {n = 1 } --> OK
  testParse("testcase5", While(Constant(1), Assignment(Variable("n"), Constant(1))), true);

  //while(1 = n ) { n = 1} --> error
  testParse("testcase6", While(Assignment(Constant(1), Variable("n")), Assignment(Variable("n"), Constant(1))), false);

  //while( n.next ) { n = 1 } --> OK
  testParse("testcase7", While(Selection(Variable("n"), "next"), Assignment(Variable("n"), Constant(1))), true);

  //while( n.next ) { 1 = n } --> error
  testParse("testcase8", While(Selection(Variable("n"), "next"), Assignment(Constant(1), Variable("n"))), false);

  //while( while(n){1}) { n = 1 } --> error
  testParse("testcase9", While(While(Variable("n"), Constant(1)), Assignment(Variable("n"), Constant(1))), false);

  //assignment statements themselves cannot be used as l-values (only as r-values)
  // (n = 2) = v
  // it is error

  testParse("testcase10", Assignment(Assignment(Variable("n"), Constant(2)), Variable("v")), false);

  // v = (n = 2)
  // it is OK
  testParse("testcase11", Assignment(Variable("v"), Assignment(Variable("n"), Constant(2))), true);

  //test assignment in Sequence
  testParse("testcase12", Sequence(Assignment(Constant(1), Variable("v"))), false);

  //test assignment in plus
  testParse("testcase13", Plus(Assignment(Constant(1), Variable("v")), Constant(0)), false);

  //test  assignment in minus
  testParse("testcase14", Minus(Assignment(Constant(1), Variable("v")), Constant(0)), false);

  //test  assignment in times
  testParse("testcase15", Times(Assignment(Constant(1), Variable("v")), Constant(0)), false);

  //test assignment in div
  testParse("testcase16", Div(Assignment(Constant(1), Variable("v")), Constant(0)), false);

  //test assignment in Selection
  testParse("testcase17", Selection(Assignment(Constant(1), Variable("v")), "next"), false);

  //test { 0 }
  testParse("testcase18", Sequence(Constant(0)), true);
  
}
