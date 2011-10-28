package luc

import SimpleImperative.{ Store, Value }

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
 * The top-level interface for a Statement composite hierarchy that
 * supports visitors.
 */

trait Statement


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
  require(!statements.contains(null))
}

case class While(guard: Statement, body: Statement) extends BinaryStatement(guard, body)

case class Assignment(left: Statement, right: Statement) extends BinaryStatement(left, right)

/**
 * Syntax for statements for creating and using records.
 */
case class New(clazz: Clazz) extends Statement {
  require(clazz != null)
}
case class Selection(receiver: Statement, field: String) extends Statement {
  require(receiver != null)
  require(field != null)
}

/**
 * Syntax for record types. Not part of the Statement hierarchy
 * because they appear only as arguments to New Statements.
 */
case class Clazz(fields: String*) {
  require(fields != null)
  require(!fields.contains(null))
}

/**
 * A cell for storing a value (either a number or an object).
 */
case class Cell(var value: Value) {
  def get = value
  def set(value: Value) = { this.value = value; this }
}
/**
 * A companion object defining a useful Cell instance.
 */
object Cell {
  def apply(i: Int): Cell = Cell(Left(i)) // Left -> number, Right -> object
  val NULL = Cell(0)
}

object GlobalStore {
  private var store: Store = Map[String, Cell]() 
  def Reset(): Unit = store = Map[String, Cell]()
  def Memory: Store = store
  def New(s: String): Cell = {
    if (!store.keySet.exists(key => key.equals(s))) {
      store += (s -> Cell(0))
    }
    store(s)
  }
  def Count: Int = store.count( s => true )
  def Watch: Unit = println(store)
}

/**
 * An interpreter for expressions and statements.
 */
object SimpleImperative {

  /**
   * A memory store is a mapping from variable names to storage cells.
   */
  type Store = Map[String, Cell]

  /**
   * An object (instance) is the same as a memory store.
   */
  type Instance = Store

  /**
   * A run-time value is either a number or an object.
   */
  type Value = Either[Int, Instance]

  def apply(store: Store)(s: Statement): Cell = s match {
    case Constant(value) => Cell(Left(value))
    case Plus(left, right) => binaryOperation(store, left, right, _ + _)
    case Minus(left, right) => binaryOperation(store, left, right, _ - _)
    case Times(left, right) => binaryOperation(store, left, right, _ * _)
    case Div(left, right) => binaryOperation(store, left, right, _ / _)
    case Variable(name) => store(name)
    case Assignment(left, right) => {
      val rvalue = apply(store)(right)
      val lvalue = apply(store)(left)
      lvalue.set(rvalue.get)
    }
    case Sequence(statements @ _*) =>
      //may be c should become left value here??
      //or only apply to clazz field?
      //??
      //I think it is only the left one
      //such as statements.count = 1
      statements.foldLeft(Cell.NULL)((c, s) =>apply(store)(s))
    case While(guard, body) => {
      var gvalue = apply(store)(guard)
      while (gvalue.get.isRight || gvalue.get.left.get != 0) {
        apply(store)(body)
        gvalue = apply(store)(guard)
      }
      Cell.NULL
    }
    case New(Clazz(fields @ _*)) =>
      // create an object based on the list of field names in the clazz
      Cell(Right(Map(fields.map(field => (field, Cell(0))): _*)))
    case Selection(record, field) => {
      // assume the expression evaluates to a record (.right)
      // and choose the desired field
      apply(store)(record).get.right.get.apply(field)
    }
  }

  def binaryOperation(store: Store, left: Statement, right: Statement, operator: (Int, Int) => Int): Cell = {
    val l: Int = evaluate(apply(store)(left))
    val r: Int = evaluate(apply(store)(right))
    Cell(Left(operator(l, r)))
  }

  def evaluate(cell: Cell): Int = cell.get.left.get
  def getCell(store: Store, v: Variable): Cell = store(v.name)
  def getValue(store: Store, v: Variable): Int = evaluate(store(v.name))

}
 
