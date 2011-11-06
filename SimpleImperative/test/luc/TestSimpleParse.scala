package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import StatementParser._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class TestSimpleParse extends FunSuite {
  def testParseAll(description: String, v: Statement, input: Array[String]) = {
    test(description) {

      val arr = input.takeWhile(s => s.length() > 0)
        .map(s => StatementParser.parseAll(StatementParser.expr, s).get).toArray

      val parseStatement: Statement = new Sequence(arr :  _*)

      println(parseStatement)

      assert(parseStatement === v)
    }
  }

  def testParse(description: String, v: Statement, s: String) = {
    test(description) {
      val parsedExpr = StatementParser.parseAll(StatementParser.expr, s)
      println(parsedExpr)
      assert(parsedExpr.get === v)
    }
  }

  def testClass(description: String, c: Clazz, s: String) = {
    test(description) {
      val parseClazz = StatementParser.parseAll(StatementParser.clazz, s)
      println(parseClazz)
      assert(parseClazz.get === c)
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
    "q" -> Cell(5),
    "s" -> Cell(0))

  var varR: Variable = new Variable("r");
  var s: Statement = new Assignment(varR, complex1)

  SimpleImperative.apply(store)(s)

  testValue("testcase2", SimpleImperative.getCell(store, varR), -3)

  var varQ = new Variable("q")
  testParse("testcase3", varQ, "q")
  testParse("testcase4", varQ, "(q)")

  testParse("testcase5", Sequence(Constant(1)), "{ 1 }")
  testParse("testcase6", Sequence(Sequence(Variable("q"))), "{ {q} }")
  testParse("testcase7", Assignment(Variable("s"), Minus(Variable("q"), Constant(1))), "s = q - 1 ")
  testParse("testcase8", Times(Plus(Variable("s"), Constant(1)), Constant(4)), " (s + 1) * 4")

  val exp2 = While(varQ, Sequence(Assignment(varQ, Minus(varQ, Constant(1)))))
  val complex1string2 = "while( q ) { q = q - 1 }"

  testParse("testcase9", exp2, complex1string2);

  SimpleImperative.apply(store)(exp2)
  testValue("testcase10", SimpleImperative.getCell(store, varQ), 0)

  testClass("testcase11", Clazz("course1", "course2"), "struct StudentSemesterRecord { course1, course2 }")

  testParse("testcase12", New(Clazz("course1", "course2")), "new struct StudentSemesterRecord { course1, course2 }")

  testClass("testcase13", Clazz("value", "next"), "struct ListNode { value, next } \n")

  //test selection
  testParse("testcase14", Selection(Variable("n"), "next"), "n.next ")

  testParse("testcase15", Assignment(Variable("n"), Selection(Variable("n"), "next")), "n = n.next ")

  //test group
  val input: Array[String] = "var n\n n = new struct ListNode { value, next } \n n = n.next ".split("\n")
  testParseAll("testcase16", Sequence(Variable("n"), Assignment(Variable("n"), New(Clazz("value", "next"))), Assignment(Variable("n"), Selection(Variable("n"), "next"))), input)
}