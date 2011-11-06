package luc

import StatementParser._
import SimpleImperative._

object mainSimple {

  def main(args: Array[String]) {

    GlobalStore.Watch

//    Iterator.continually(Console.readLine).takeWhile(_ != "quit;")
//    .foreach(line => match s (
//        
//        )
//      )

    for (s <- args) {
      val parsedExpr = StatementParser.parseAll(StatementParser.expr, s)
      SimpleImperative.apply(GlobalStore.Memory)(parsedExpr.get)
    }

  }

}