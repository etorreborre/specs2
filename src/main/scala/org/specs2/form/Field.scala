package org.specs2
package form

import scala.xml._
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
  /** executing a field does nothing */
  override def execute = success
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T) = Field(label, value(v))
  /** @return the field value */
  def apply(): T = value.get
  /** alias for apply() */
  def get: T = apply()
  /** @return "label: value" */
  override def toString = if (label.nonEmpty) label + ": " + this.get else this.get.toString
  /** transforms this typed Field as a Field containing the toString value of the Fields value*/
  def toStringField = Field(label, value.get.toString)
  /** set a new Decorator */
  def decorateWith(f: Any => Any) = Field(label, value, decorator.decorateWith(f))
  /** set a new Decorator for the label */
  def decorateLabelWith(f: Any => Any) = Field(label, value, decorator.decorateLabelWith(f))
  /** set a new Decorator for the value */
  def decorateValueWith(f: Any => Any) = Field(label, value, decorator.decorateValueWith(f))
  /** set a new style */
  def styleWith(s: (String, String)) = Field(label, value, decorator.styleWith(s))
  /** set a new style for the label */
  def styleLabelWith(s: (String, String)) = Field(label, value, decorator.styleLabelWith(s))
  /** set a new style for the value */
  def styleValueWith(s: (String, String)) = Field(label, value, decorator.styleValueWith(s))
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

trait DecoratedProperty[T] {
  val decorator: Decorator
  /** set a new Decorator */
  def decorateWith(f: Any => Any): T
  /** set a new Decorator for the label */
  def decorateLabelWith(f: Any => Any): T
  /** set a new Decorator for the value */
  def decorateValueWith(f: Any => Any): T
  /** do the decoration */
  def decorateLabel(ns: Any) = decorator.label(ns)
  /** do the decoration */
  def decorateValue(ns: Any) = decorator.value(ns)
  /** set a new style */
  def styleWith(s: (String, String)): T
  /** set a new style for the label */
  def styleLabelWith(s: (String, String)): T
  /** set a new style for the value */
  def styleValueWith(s: (String, String)): T

  def labelStyles = decorator.labelStyles.mkString("; ")
  def valueStyles = decorator.valueStyles.mkString("; ")

}
case class Decorator(label: Any => Any = identity, value: Any => Any = identity,
                     labelStyles: List[String] = Nil, valueStyles: List[String] = Nil) {
  def decorateWith(f: Any => Any) = copy(label = f compose label, value = f compose value)
  def decorateLabelWith(f: Any => Any) = copy(label = f compose label)
  def decorateValueWith(f: Any => Any) = copy(value = f compose value)

  def styleWith     (s: (String, String)) = copy(labelStyles = labelStyles :+ (s._1+":"+s._2), valueStyles = valueStyles :+ (s._1+":"+s._2))
  def styleLabelWith(s: (String, String)) = copy(labelStyles = labelStyles :+ (s._1+":"+s._2))
  def styleValueWith(s: (String, String)) = copy(valueStyles = valueStyles :+ (s._1+":"+s._2))
}