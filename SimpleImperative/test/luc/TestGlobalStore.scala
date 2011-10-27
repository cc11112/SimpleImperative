package luc

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import TestFixtures._
import SimpleImperative._

@RunWith(classOf[JUnitRunner])
class TestGlobalStore extends FunSuite {

  def testValue(description: String, cell: Cell, result: Int) = {
    test(description) {
      assert(SimpleImperative.evaluate(cell) === result)
    }
  }

  def testCount(description: String, count: Int, result: Int) = {
    test(description) {
      assert(count === result)
    }
  }

  val cell: Cell = GlobalStore.New("s")
  cell.set(Left(12))
  testValue("test case 1", cell, 12)

  GlobalStore.Reset
  GlobalStore.Watch

  testCount("test count", GlobalStore.Count, 0)
  
  GlobalStore.New("s2").set(Left(4))
  GlobalStore.New("s3").set(Left(7))
  GlobalStore.Watch
  testCount("test count 2", GlobalStore.Count, 2)
}