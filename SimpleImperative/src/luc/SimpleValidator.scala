package luc

object SimpleValidator {
  def Check(s: Statement): Boolean = s match {
    case While(guard, body) => guard match {
      case Constant(c) => Check(body)
      case Variable(v) => Check(body)
      case Assignment(left, right) => Check(guard) && Check(body)
      case Selection(receiver, field) => Check(body)
      case Sequence(statements @ _*) => Check(statements.last) && Check(body)
      case _ => false
    }
    case Assignment(left, right) => left match {
      case Variable(v) => right match {
        case While(guard, body) => false
        case e: Assignment => Check(e)
        case _ => true
      }
      case Sequence(statements @ _*) => statements.map(s => Check(s)).foldLeft(true)(_ && _)
      case Assignment(left, right) => Left(left).isRight && Check(right)
      case Selection(left, field) => Check(left) //only check left is enough, because we have not read only field
      case _ => false
    }
    case Sequence(statements @ _*) => statements.map(s => Check(s)).foldLeft(true)(_ && _)
    case Plus(left, right) => Check(left) && Check(right)
    case Minus(left, right) => Check(left) && Check(right)
    case Times(left, right) => Check(left) && Check(right)
    case Div(left, right) => Check(left) && Check(right)
    case Selection(r, f) => Check(r)
    case Variable(n) => true
    case Constant(e) => true
    case New(e) => true
    case null => true
    case _ => false
  }
}