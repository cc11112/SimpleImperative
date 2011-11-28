package luc

import scala.collection.mutable.ListBuffer
import StatementParser._
import SimpleImperative._

object mainSimple {

  var input = new ListBuffer[String]

  def Repl(s: String): Unit = s match {
    case "quit;" => println("exit."); System.exit(0)
    case "help;" => println("ouput usage text here")
    case "dump;" => GlobalStore.Watch
    case "clear;" => GlobalStore.Reset; println("all memory variables have been cleared.")
    case _ => {
      if (s.endsWith(";")) {
        val line = s.replace(';', ' ').trim()
        if (line.length() > 0)
          input += (line + "\n")
        Execute()
        input.clear()
      } else {
        input += s
      }
    }
  }

  def Parse(s: String): Statement = {
    try {
      StatementParser.parseAll(StatementParser.value, s).get match {
        case c: Clazz => null
        case s: Statement => s
        case Some(null) ~ Some(s: Statement) => s
        case Some(Clazz) ~ None => null
        case Failure => null
      }
    } catch {
      case e: Exception => println(e.getMessage()); null
    }
  }

  def Parse(): Statement = {
    val arr = input.filter(s => s.trim() != "").map(s => Parse(s)).filter(e => e != null).toArray
    if (arr.length == 0) {
      println("parse expression error!")
      null
    } else {
      if (arr.length == 1) arr.head else new Sequence(arr: _*)
    }
  }

  def Execute(): Boolean = {

    try {
      val parseStatement = Parse()
      if (SimpleValidator.Check(parseStatement)) {
        if (parseStatement != null) {
          //only for debug
          //println(parseStatement)
          GlobalStore.Allocation(parseStatement)
          //GlobalStore.Watch
          SimpleImperative.apply(GlobalStore.Memory)(parseStatement)
        }
        return true
      } else {
        System.err.println("Valid Check error!")
      }
    } catch {
      case e: Exception => println(e.getMessage());
    }
    false
  }

  def main(args: Array[String]) {
    
    GlobalStore.Watch
    Iterator.continually(Console.readLine).foreach(line => Repl(line.trim()))
  }

}