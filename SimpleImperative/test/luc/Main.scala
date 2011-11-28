package luc

import java.io._
import scala.util.control.Breaks._

object Main {
 def main(args : Array[String]) : Unit = {
   
    //user breakable
    val in = new BufferedReader(new InputStreamReader(System.in))
    
    breakable{
      while(true){
        println("? ")
        if (in.readLine() == "") break
      }
    }
    
    
    println("test")
  }
}