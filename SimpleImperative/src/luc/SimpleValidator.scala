package luc

object SimpleValidator {
  def Check(s: Statement): Boolean = s match {
    case While(guard,body ) => guard match {
      case Constant(c) => true
      case Variable(v) => true
      case Assignment(left, right) => Check(right)
      case Selection(receiver,  field) => true
      case Sequence(statements @ _*) => Check(statements.last)
      case _ => false
    }
    case Assignment(left, right) => left match {
       case Variable(v) => true
       case Sequence(statements @ _*) => Check(statements.last)
       case Assignment(left, right) => Left(left).isRight 
       case _ => false
    }
    case _ => true
  }
}