package luc

/**
 * The top-level interface for a Statement composite hierarchy that
 * supports visitors.
 */

trait Statement   

/**
 * Something that can be used on the right-hand side of an assignment.
 */
trait RValue[T] {
  def get: T
}

/**
 * Something that can be used on the left-hand side of an assignment.
 */
trait LValue[T] extends RValue[T] {
  def set(value: T): LValue[T]
}

/**
 * A binary statement with two non-null children.
 */
abstract class BinaryStatement(left: Statement, right: Statement) extends Statement {
  require(left != null)
  require(right != null)
}

/**
 * Applicative (side-effect-free) statements.
 */
case class Constant(value: Int) extends Statement
case class Plus(left: Statement, right: Statement) extends BinaryStatement(left, right)
case class Minus(left: Statement, right: Statement) extends BinaryStatement(left, right)
case class Times(left: Statement, right: Statement) extends BinaryStatement(left, right)
case class Div(left: Statement, right: Statement) extends BinaryStatement(left, right)

/**
 * Imperative statements, that is, those that are interesting because of their
 * side effects.
 */
case class Variable(name: String) extends Statement {
  require(name != null)
}
case class Sequence(statements: Statement*) extends Statement {
  require(statements != null)
  require(! statements.contains(null))
}
case class While(guard: Statement, body: Statement) extends BinaryStatement(guard, body)
case class Assignment(left: Statement, right: Statement) extends BinaryStatement(left, right)



/**
 * A cell for storing a value.
 */
case class Cell[T](var value: T) extends LValue[T] {
  override def get = value
  override def set(value: T) = { this.value = value ; this }
}

/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  val NULL = Cell(0)
}

/**
 * An interpreter for expressions and statements.
 */
object SimpleImperative {

  type Store = Map[String, LValue[Int]]

  def apply(store: Store)(s: Statement): LValue[Int] = s match {
    case Constant(value) => Cell(value)
    case Plus(left, right) => Cell(apply(store)(left).get + apply(store)(right).get)
    case Minus(left, right) => Cell(apply(store)(left).get - apply(store)(right).get)
    case Times(left, right) => Cell(apply(store)(left).get * apply(store)(right).get)
    case Div(left, right) => Cell(apply(store)(left).get / apply(store)(right).get)
    case Variable(name) => store(name)
    case Assignment(left, right) => {
      val lvalue = apply(store)(left)
      val rvalue = apply(store)(right)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      statements.foldLeft(Cell.NULL.asInstanceOf[LValue[Int]])((c, s) => apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
  }
}
 