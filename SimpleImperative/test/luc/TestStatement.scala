package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import TestFixtures._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class TestStatement extends FunSuite {

  def testValue(description: String, cell: Cell, result: Int) = {
    test(description) {
      assert(SimpleImperative.evaluate(cell) === result)
    }
  }

  //test case 1
  val store = Map[String, Cell](
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

  testValue("testcase1", SimpleImperative.getCell(store, varR), 6)

  //test case 2
  val studentCourseRecord = Clazz("firstExamScore", "secondExamScore", "totalScore")
  val studentSemRecord = Clazz("course1", "course2")

  val store2 = Map[String, Cell](
    "q" -> Cell(0),
    "r" -> Cell(0))

  val s2 =
    Sequence(
      Assignment(Variable("r"), New(studentSemRecord)),
      Assignment(Selection(Variable("r"), "course1"), New(studentCourseRecord)),
      Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(25)),
      Assignment(Selection(Selection(Variable("r"), "course1"), "secondExamScore"), Constant(35)),
      Assignment(Selection(Selection(Variable("r"), "course1"), "totalScore"),
        Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
          Selection(Selection(Variable("r"), "course1"), "secondExamScore"))),
      Assignment(Selection(Variable("r"), "course2"), Selection(Variable("r"), "course1")),
      Assignment(Variable("q"), Selection(Selection(Variable("r"), "course2"), "totalScore")),
      Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(45)))

  SimpleImperative.apply(store2)(s2)

  testValue("testcase2", store2("q"), 60)

  //test case 3
  val store3 = Map[String, Cell](
    "n" -> Cell(0),
    "h" -> Cell(0),
    "s" -> Cell(0))

  val listNode = Clazz("value", "next")

  val s3 =
    Sequence(
      Assignment(Variable("n"), New(listNode)),
      Assignment(Variable("h"), Variable("n")),
      Assignment(Selection(Variable("n"), "value"), Constant(2)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(3)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(5)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(7)),
      Assignment(Selection(Variable("n"), "next"), Constant(0)),
      Assignment(Variable("n"), Variable("h")),
      While(Variable("n"),
        Sequence(
          Assignment(Variable("s"), Plus(Variable("s"), Selection(Variable("n"), "value"))),
          Assignment(Variable("n"), Selection(Variable("n"), "next")))))

  SimpleImperative.apply(store3)(s3)

  testValue("testcase3", store3("s"), 17)

  //test case 4
  val store4 = Map[String, Cell](
    "n" -> Cell(0),
    "s" -> Cell(0))

  val s4 = Sequence(
    Assignment(Variable("n"), New(listNode)),
    Assignment(Selection(Variable("n"), "value"), Constant(5)),
    Assignment(Selection(Variable("n"), "next"), Constant(0)),
    Assignment(Sequence(
      Assignment(Selection(Variable("n"), "value"), Constant(7)),
      Selection(Variable("n"), "next")), Constant(12)))

  SimpleImperative.apply(store4)(s4)
  
  println(store4("n"))

  var s5 = Sequence(Assignment(Variable("s"), Selection(Variable("n"), "next")))

  SimpleImperative.apply(store4)(s5)

  println(store4("s"))

  testValue("testcase4", store4("s"), 12)
  
}