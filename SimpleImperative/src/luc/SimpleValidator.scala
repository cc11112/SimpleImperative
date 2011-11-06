package luc

object SimpleValidator {
  def Check(s: Statement): Boolean = s match {
    case w: While => w.guard match {
      case Constant(c) => true
      case Variable(v) => true
      case Assignment(left, right) => Check(right)
      case Selection(receiver,  field) => true
      case Sequence(statements @ _*) => Check(statements.last)
      case _ => false
    }
    case a: Assignment => a.left match {
       case Variable(v) => true
       case Sequence(statements @ _*) => Check(statements.last)
       case Assignment(left, right) => Left(left).isRight 
       case _ => false
    }
    case _ => true
  }
}