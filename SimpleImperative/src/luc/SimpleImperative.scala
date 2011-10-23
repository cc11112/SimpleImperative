package luc

/**
 * The top-level interface for a Statement composite hierarchy that
 * supports visitors.
 */

trait Statement {
  //  def accept(v: StatementVisitor[Result] ) : Result 
}

abstract class AbstractStatement extends Statement

/**
 * The interface for visitors over Statements.  There is one visitation
 * method for each concrete implementation class of Statement.  Note that
 * we no longer distinguish between expressions and statements.  Such a
 * distinction could be made part of the type system.
 */

//trait StatementVisitor[Result] {
//    def visitPlus(s : Plus ) : Result
//    def visitMinus(s: Minus) : Result
//    def visitConstant(s: Constant): Result
//    def visitVariable(s : Variable): Result
//    def visitSequence(s : Sequence): Result
//    def visitWhile(s: While): Result
//    def visitAssignment(s : Assignment): Result
//    def visitBreakpoint(s : Breakpoint): Result
//}

trait LValue {
  def set(v: Int): Unit
  def get(): Int
}

class Variable(v: Int) extends AbstractStatement with LValue {
  var value: Int = v 

  def get(): Int = value
  def set(value: Int) = this.value = value

}

case class Constant(v: Int) extends Variable(v) {
  override def set(value: Int) = {}
}

class BaseStatement(l: Statement, r: Statement) extends AbstractStatement{
  var lt: Statement = l
  var rt: Statement = r

  def getLeft(): Statement = lt
  def getRight(): Statement = rt
}

case class Plus(l: Statement, r: Statement) extends BaseStatement(l, r)

case class Minus (l: Statement, r: Statement) extends BaseStatement(l, r)

case class Assignment (l: Statement, r: Statement) extends BaseStatement(l, r)

case class Sequence (l: Statement, r: Statement) extends BaseStatement(l, r)

case class While (l: Statement, r: Statement) extends BaseStatement(l, r)


object SimpleImperative {
  def Count(s: String): Int = {
    1
  }
}

 