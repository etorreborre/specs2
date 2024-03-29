package org.specs2
package form

import control.Property
import control.Properties.*
import execute.*
import matcher.*
import text.NotNullStrings.*

/** The Prop class is a named property which holds:
  *   - an actual value
  *   - an expected value
  *   - a constraint to check if the actual value conforms to the expected one
  *
  * This property can be executed and can be inserted in a Form.
  *
  * A Prop is meant to be declared as "bound" to an actual value:
  *
  * `val customerName = Prop("Customer name", person.name)`
  *
  * [the actual value is not evaluated until the Prop is executed]
  *
  * Then it can be associated an expected value with the apply method (usually in a Form declaration):
  *
  * `customerName("Bill")`
  *
  * The actual and the expected values can have different types and the constraint which is applied to them can be
  * anything returning a result.
  *
  * However the Prop companion object provides a method to create a Property with a constraint using a beEqualTo
  * matcher:
  *
  * `Prop("Name", "Eric")("Eric") must ===(Success("'Eric' is equal to 'Eric'"))`
  */
case class Prop[T, S](
    label: String = "",
    actual: Property[T] = Property[T](),
    expected: Property[S] = Property[S](),
    constraint: (T, S) => Result = Prop.checkProp,
    decorator: Decorator = Decorator().bkGreyLabel
) extends Executable
    with DecoratedProperty[Prop[T, S]]:

  /** The apply method sets the expected value and returns the Prop
    */
  def apply(e: =>S): Prop[T, S] = new Prop(label, actual, expected(e), constraint)

  /** @return the actual value as either Right(value) or Left(result) */
  lazy val actualValue = ResultExecution.executeProperty(actual, Pending("No actual value"))

  /** @return the expected value as an option */
  lazy val expectedValue = ResultExecution.executeProperty(expected, Pending("No expected value"))

  /** execute the constraint set on this property, with the expected value */
  def execute: Result =
    val result = for
      a <- actualValue.toOption
      e <- expectedValue.toOption
    yield ResultExecution.execute(constraint(a, e))
    result.getOrElse(expectedValue.left.toOption.getOrElse(actualValue.left.toOption.get))

  /** set a specific result on the property
    */
  def resultIs(r: =>Result): Prop[T, S] =
    copy(constraint = (t: T, s: S) => r)

  /** set a specific constraint between the actual and expected value
    */
  def matchWith(c: (T, S) => Result): Prop[T, S] =
    copy(constraint = c)

  /** Display the property:
    *
    * label: "this" (actual: "that")
    */
  override def toString =
    (if label.isEmpty then "" else label + ": ") +
      valueToString(actualValue) +
      (if expected.toOption.isEmpty || expectedValue.toOption == actualValue.toOption then ""
       else " (expected: " + valueToString(expectedValue) + ")")

  /** @return
    *   the string for the expected/actual value depending on its existence and execution result
    */
  private def valueToString(executed: Either[Result, ?]) =
    executed match
      case Right(r)         => r.notNull
      case Left(Pending(_)) => "_"
      case Left(r)          => r.toString

  /** set a new Decorator */
  def decoratorIs(d: Decorator): Prop[T, S] =
    copy(decorator = d)

  override def equals(other: Any) = other.asInstanceOf[Matchable] match
    case Prop(l, a, e, c, d) => label == l && actual == a && expected == e
    case _                   => false

  override def hashCode = label.hashCode + actual.hashCode + expected.hashCode

/** Companion object with factory methods
  */
object Prop:
  /** create a Prop with a label and an actual value */
  def apply[T](label: String, actual: =>T): Prop[T, T] =
    new Prop(label, Property(actual), Property[T](), checkProp)

  /** create a Prop with a label, an actual value, and a constraint */
  def apply[T, S, R: AsResult](label: String, act: =>T, c: (T, S) => R): Prop[T, S] =
    new Prop[T, S](label, actual = Property(act), constraint = (t, s) => AsResult(c(t, s)))

  /** create a Prop with a label, an expected value, and a constraint */
  def apply[T, S](label: String, act: =>T, c: S => Matcher[T]): Prop[T, S] =
    new Prop[T, S](label, actual = Property(act), constraint = (t: T, s: S) => c(s).apply(createExpectable(t)))

  /** create a Prop with a label, an actual value, and a matcher on the actual value */
  def apply[T](label: String, act: =>T, c: Matcher[T]): Prop[T, T] =
    lazy val a = act
    Prop[T, T](label, a, a, c)

  /** create a Prop with a label, an actual value, an expected value, and a constraint on the actual value */
  def apply[T, S](label: String, act: =>T, exp: =>S, c: Matcher[T]): Prop[T, S] =
    new Prop[T, S](
      label,
      actual = Property(act),
      expected = Property(exp),
      constraint = (t: T, s: S) => c(createExpectable(t))
    )

  /** create a Prop with a label */
  def apply[T](label: String): Prop[T, T] =
    new Prop[T, T](label = label)

  /** create a Prop with an empty label and an actual value */
  def apply[T](act: =>T): Prop[T, T] =
    new Prop[T, T](actual = Property(act))

  /** default constraint function */
  private[Prop] def checkProp[T, S]: (T, T) => Result = (t: T, s: T) => new BeEqualTo(s).apply(createExpectable(t))

trait PropSyntax:

  extension [T, R: AsResult](p: Prop[T, T])
    /** check the actual value with a function
      */
    def checkWith(f: T => R): Prop[T, T] =
      p.matchWith((t, _) => AsResult(f(t)))

  extension [T](p: Prop[T, T])
    /** check the actual value with a matcher
      */
    infix def must(m: Matcher[T]): Prop[T, T] =
      // We set expected to actual since the expected value is not used in the constraint
      p.copy(expected = p.actual).matchWith((t, _) => m.apply(createExpectable(t)))

/** generic trait for anything having a label, to unify Props and Forms
  */
trait HasLabel:
  val label: String
