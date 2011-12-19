package org.specs2
package form

import control.{ Property }
import control.Exceptions._
import execute._
import matcher._
import MatchersImplicits._
import text.NotNullStrings._

/**
 * The Prop class is a named property which holds:
 *   - an expected value
 *   - an actual value
 *   - a constraint to check if the actual value conforms to the expected one
 * 
 * This property can be executed and can be inserted in a Form.
 *
 * A Prop is meant to be declared as "bound" to an actual value:
 *
 *   `val customerName = Prop("Customer name", person.name)`
 * 
 * [the actual value is not evaluated until the Prop is executed]
 * 
 * Then it can be associated an expected value with the apply method (usually in a Form declaration):
 * 
 *   `customerName("Bill")`
 * 
 * The actual and the expected values can have different types and the constraint which is 
 * applied to them can be anything returning a result.
 * 
 * However the Prop companion object provides a method to create a Property with a constraint
 * using a beEqualTo matcher:
 * 
 * `Prop("Name", "Eric")("Eric") must_== Success("'Eric' is equal to 'Eric'")`
 *
 */
case class Prop[T, S](
              label: String = "",
              actual: Property[T] = Property[T](),
              expected: Property[S] = Property[S](),
              constraint: (T, S) => Result = Prop.checkProp,
              decorator: Decorator = Decorator()) extends Executable with DecoratedProperty[Prop[T, S]] {
  
  /**
   * The apply method sets the expected value and returns the Prop
   */
  def apply(e: =>S): Prop[T, S] = new Prop(label, actual, expected(e), constraint)
  /** @return the actual value as either Right(value) or Left(result) */
  lazy val actualValue = ResultExecution.executeProperty(actual, Pending("No actual value"))
  /** @return the expected value as an option */
  lazy val expectedValue = ResultExecution.executeProperty(expected, Pending("No expected value"))
  /**
   * shortcut method for this().get returning the contained expected value.
   * @return the expected value if set and throws an exception otherwise
   */
  def get: S = expected.get

  /** execute the constraint set on this property, with the expected value */
  def execute: Result = {
    val result = for {
      a <- actualValue.right.toOption
      e <- expectedValue.right.toOption
    } yield ResultExecution.execute(constraint(a, e))
    result.getOrElse(expectedValue.left.toOption.getOrElse(actualValue.left.toOption.get))
  }

  /**
   * set a specific result on the property
   */
  def resultIs(r: =>Result): Prop[T, S] = copy(constraint = (t: T, s: S) => r)
  /**
   * set a specific constraint on the property
   */
  def matchWith(c: (T, S) => Result): Prop[T, S] = copy(constraint = c)

  /**
   * Display the property:
   * 
   * label: "this" (actual: "that")
   */
  override def toString = {
    (if (label.isEmpty) "" else (label + ": ")) + 
    valueToString(expectedValue) +
    (if (expectedValue.right.toOption == actualValue.right.toOption) "" else (" (actual: " + valueToString(actualValue) + ")"))
  }

  /**
   * @return the string for the expected/actual value depending on its existence and execution result
   */
  private def valueToString(executed: Either[Result, _]) = {
    executed match {
      case Right(r)          => r.notNull
      case Left(Pending(_))  => "_"
      case Left(r)           => r.toString
    }
  }
  /** set a new Decorator */
  def decoratorIs(d: Decorator) = copy(decorator = d)

  override def equals(a: Any) = a match {
    case Prop(l, a, e, c, d) => label == l && actual == a && expected == e
    case other               => false
  }
  override def hashCode = label.hashCode + actual.hashCode + expected.hashCode
}
/**
 * Companion object with factory methods
 */
object Prop {
  /** create a Prop with a label and an expected value */
  def apply[T](label: String, actual: =>T): Prop[T, T] = {
    new Prop(label, Property(actual), Property[T](), checkProp)
  }
  /** create a Prop with a label, an expected value, and a constraint */
  def apply[T, S](label: String, act: =>T, c: (T, S) => Result) = {
    new Prop[T, S](label, actual = Property(act), constraint = c)
  }
  /** create a Prop with a label, an expected value, and a constraint */
  def apply[T, S](label: String, act: =>T, c: (S) => Matcher[T]) = {
    new Prop[T, S](label, actual = Property(act), constraint = (t: T, s: S) => c(s).apply(Expectable(t)).toResult)
  }
  /** create a Prop with a label, an actual value, and a matcher on the actual value */
  def apply[T](label: String, act: =>T, c: Matcher[T]): Prop[T, T] = {
    lazy val a = act
    Prop[T, T](label, a, a, c)
  }
  /** create a Prop with a label, an actual value, an expected value, and a constraint on the actual value*/
  def apply[T, S](label: String, act: =>T, exp: =>S, c: Matcher[T]): Prop[T, S] = {
    new Prop[T, S](label, actual = Property(act), expected = Property(exp), constraint = (t: T, s: S) => c(Expectable(t)).toResult)
  }
  /** create a Prop with an empty label and an actual value */
  def apply[T](act: =>T): Prop[T, T] = new Prop[T, T](actual = Property(act))
  /** default constraint function */
  private[Prop] def checkProp[T, S]: (T, T) => Result = (t: T, s: T) => (new BeTypedEqualTo(s).apply(Expectable(t))).toResult
}
/**
 * generic trait for anything having a label, to unify Props and Forms
 */
trait HasLabel {
  val label: String
}