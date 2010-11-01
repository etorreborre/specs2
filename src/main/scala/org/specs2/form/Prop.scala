package org.specs2
package form

import execute._
import StandardResults._
import matcher._

/**
 * The Prop class is a named property which holds:
 *   * an expected value
 *   * an actual value
 *   * a constraint to check if the actual value conforms to the expected one
 * 
 *
 * This property can then be executed and can be inserted in the Layout of a Form.
 *
 * A Prop property is meant to be declared as "bound" to an actual value:<code>
 *
 *   val customerName = Prop("Customer name", person.name)
 * </code>
 * 
 * [the actual value is not evaluated until the Prop is executed]
 * 
 * Then it can be associated an expected value with the apply method (usually in a Form declaration):
 * <code>
 *   customerName("Bill")
 * </code>
 * 
 * Different constraints can be set on a Prop, by using the companion object factory methods:<code>
 *
 * // build a Prop with an AnyConstraint. The block will be executed if the property is executed 
 * val propWithABlock = Prop("label", actualValue, thisVariableInTheScope must_== thatVariableInTheScope)
 *
 * // build a Prop with a FunctionConstraint. The function will be executed with the actual and expected values if the property is executed
 * val propWithAFunction = Prop("label", actualValue, (actual, expected) => actual must_== expected)
 *
 * // build a Prop with a MatcherConstraint. The function will be executed with the default matcher (BeEqualTo)
 * //  if the property is executed
 * val propWithAMatcherExecutor = Prop("label", actualValue, m => actual must m)
 *
 * // Note that in that case the matcher set on the constraint can be changed with
 * propWithAMatcherExecutor.matchesWith(beGreaterThan(_))
 *
 * </code>
 * 
 * Props can be temporarily commented out with the comment() method and thus won't be
 * executed:
 *   name("Eric").comment() // won't check the expected value
 *
 *
 */
case class Prop[T, S](val label: String = "",
              val actual: Property[T] = Property[T](), 
              val expected: Property[S] = Property[S](),
              val constraint: (T, S) => Result = Prop.checkProp) 
              extends Executable {
  
  /**
   * The apply method sets the expected value and returns the Prop
   */
  def apply(e: S): Prop[T, S] = new Prop(label, actual, expected(e), constraint)
  def actualValue = actual.optionalValue
  /** 
   * shortcut method for this().get returning the contained expected value.
   * @return the expected value if set and throws an exception otherwise
   */
  def get: S = expected.get

  /** execute the constraint set on this property, with the expected value */
  def execute = expected.flatMap { e => 
    actual.map(a => constraint(a, e)).toOption 
  }.getOrElse(pending)

  /**
   * Display the property:
   * 
   * label: "this" (actual: "that")
   */
  override def toString = {
    (if (label.isEmpty) "" else (label + ": ")) + 
    expected.getOrElse("_") + 
    (if (expected == actual) "" else (" (actual: " + actual.getOrElse("_") + ")"))
  }
}
/**
 * Companion object with factory methods
 */
object Prop {
  /** create a Prop with a label and an expected value */
  def apply[T](label: String, actual: =>T) = new Prop(label, Property(actual), Property[T](), checkProp)
  /** create a Prop with a label, an expected value, and a constraint */
  def apply[T, S](label: String, act: =>T, c: (T, S) => Result) = new Prop[T, S](label, actual = Property(act), constraint = c)
  /** create a Prop with a label, an expected value, and a constraint */
  def apply[T, S](label: String, act: =>T, c: (S) => Matcher[T]) = 
    new Prop[T, S](label, actual = Property(act), constraint = (t: T, s: S) => c(s).apply(new Expectable(t)).toResult)
  /** create a Prop with an empty label and an actual value */
  def apply[T](value: =>T) = new Prop[T, T](actual = Property(value))
  
  /** default constraint function */
  private[Prop] def checkProp[T, S]: (T, T) => Result = (t: T, s: T) => (new BeEqualTo(s).apply(new Expectable(t))).toResult
}
/**
 * generic trait for anything having a label, to unify Props and Forms
 */
trait HasLabel {
  val label: String
}