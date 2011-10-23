package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import SimpleImperative._
import TestFixtures._

@RunWith(classOf[JUnitRunner])
class TestStatement extends FunSuite {

  def testStatement(description: String, v: AbstractStatement, r: AbstractStatement) = {
    test(description) {
    	assert(v === r)
    }
  }

   testStatement("plus",
       new Plus(new Constant(1), new Constant(2)),
       new Plus(new Constant(1), new Constant(2))
   )

}