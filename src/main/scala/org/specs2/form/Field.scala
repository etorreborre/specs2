package org.specs2
package form

import scala.xml._
import control.Exceptions._
import control.Property
import execute._
import DecoratedProperties._
/**
 * A Field is a property which is used only to display input values or output values.
 * 
 * The apply method can be used to retrieve the Field value:
 *   `Field(label, 1).apply() must_== 1`
 * 
 * The value is stored in a Property object so it will not be evaluated until explicitly
 * queried.
 */
case class Field[T](label: String, value: Property[T], decorator: Decorator = Decorator()) extends Executable with StandardResults
  with DecoratedProperty[Field[T]] {
  /** executing a field execute the value and returns success unless there is an Error */
  override def execute = {
    valueOrResult match {
      case Left(e)  => e
      case Right(v) => skipped
    }
  }
  def valueOrResult: Either[Result, T] = {
    trye(value.get)(Error(_))
  }
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T) = new Field(label, value(v), decorator)
  /** @return the field value */
  def apply(): T = value.get
  /** alias for apply() */
  def get: T = apply()
  /** @return "label: value" */
  override def toString = if (label.nonEmpty) label + ": " + this.get else this.get.toString
  /** transforms this typed Field as a Field containing the toString value of the Fields value*/
  def toStringField = new Field(label, Property(value.get.toString), decorator)
  /** set a new Decorator */
  def decoratorIs(d: Decorator) = copy(decorator = d)

  /** use this Field as a header in a table */
  def header = this.center.bold.bkGrey

  override def equals(a: Any) = a match {
    case Field(l, v, _) => label == l && value == v
    case other          => false
  }
  override def hashCode = label.hashCode + value.hashCode
}
/**
 * Factory methods for creating Fields. Fields values can also be concatenated to produce
 * "summary" fields.
 * 
 * val f1 = Field(label, "hello")
 * val f2 = Field(label, "world")
 * val concatenatedFields = Field(label, f1, f2)
 * concatenatedFields.toString == label: hello/world
 * 
 * val concatenatedFields2 = Field(label, ", ", f1, f2)
 * concatenatedFields2.toString == label: hello, world
 */
case object Field {
  def apply[T](value: =>T): Field[T] = new Field("", Property(value))
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, Property(value))
  def apply(label: String, value1: Field[_], values: Field[_]*): Field[String] = Field(label, "/", value1, values:_*)
  def apply(label: String, separator: String, value1: Field[_], values: Field[_]*): Field[String] = {
    val f: Field[String] = if (values.isEmpty)
      Field(label, value1.get.toString)
    else
      Field(label, (value1 :: values.toList).map(_.get).mkString(separator))
    Field(label, f())
  }
}

