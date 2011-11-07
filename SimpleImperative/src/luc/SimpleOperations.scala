package luc

import scala.collection.mutable.ListBuffer
import StatementParser._
import SimpleImperative._

object mainSimple {

  var input = new ListBuffer[String]

  def Repl(s: String): Unit = s match {
    case "quit;" => System.exit(0)
    case "help;" => println("ouput usage text here")
    case "dump;" => GlobalStore.Watch
    case "clear;" => GlobalStore.Reset
    case _ => {
      if (s.endsWith(";")) {
        input += s.replace(';', ' ')
        Execute()
        input = new ListBuffer[String]
      } else {
        input += s
      }
    }
  }

  def Execute() = {

    val arr = input.takeWhile(s => s.length() > 0)
      .map(s => StatementParser.parseAll(StatementParser.expr, s).get).toArray

    val parseStatement: Statement = new Sequence(arr: _*)

    if (SimpleValidator.Check(parseStatement)) {
      println(parseStatement)
      SimpleImperative.apply(GlobalStore.Memory)(parseStatement)
    } else {
      System.err.println("Valid Check error!")
    }

  }

  def main(args: Array[String]) {
    GlobalStore.Watch
    Iterator.continually(Console.readLine).foreach(line => Repl(line))
  }

}