package luc

object SimpleValidator {
  def Check(s: Statement): Boolean = s match {
    case w: While => w.guard match {
      case c: Constant => true
      case v: Variable => true
      case a: Assignment => Check(a.right)
      case s: Selection => true
      case Sequence(ss @ _*) => Check(ss.last)
      case _ => false
    }
    case a: Assignment => a.left match {
       case v: Variable => true
       case Sequence(ss @ _*) => Check(ss.last)
       case as: Assignment => Right(as.left).isRight 
       case _ => false
    }
    case _ => true
  }
}