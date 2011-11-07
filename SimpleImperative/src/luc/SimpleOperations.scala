package luc

import scala.collection.mutable.ListBuffer
import StatementParser._
import SimpleImperative._

object mainSimple {

  var input = new ListBuffer[String]

  def Repl(s: String): Unit = s match {
    case "quit;" => {
       println("exit.")
       System.exit(0)
    }
    case "help;" => println("ouput usage text here")
    case "dump;" => GlobalStore.Watch
    case "clear;" => GlobalStore.Reset
    case _ => {
      if (s.endsWith(";")) {
        val line = s.replace(';', ' ').trim()
        if (line.length() > 0)
          input += (line + "\n")
        Execute()
        input = new ListBuffer[String]
      } else {
        input += s
      }
    }
  }

  def Execute() = {
	//println("Now Execute:")
   // for (s <- input)
    //  println(s)

    val arr = input.takeWhile(s => s.trim().length() > 0)
      .map(s => StatementParser.parseAll(StatementParser.expr, s).get).toArray

    val parseStatement: Statement = new Sequence(arr: _*)

    if (SimpleValidator.Check(parseStatement)) {
      println(parseStatement)

      GlobalStore.Allocation(parseStatement)

      //GlobalStore.Watch
      SimpleImperative.apply(GlobalStore.Memory)(parseStatement)
    } else {
      System.err.println("Valid Check error!")
    }

  }

  def main(args: Array[String]) {
    GlobalStore.Watch
    Iterator.continually(Console.readLine).foreach(line => Repl(line.trim()))
  }

}