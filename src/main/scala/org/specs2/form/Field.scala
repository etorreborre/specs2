package org.specs2
package form
import execute._
/**
 * A Field is a property which is used only to display input values or output values.
 * The apply method can be used to retrieve the Fields value:<code>
 *   val f = Field(label, 1)
 *   f() must_== 1
 * </code>
 * 
 * The value is stored in a Property object so it will not be evaluated until explicitly queried.
 */
case class Field[T](label: String, value: Property[T]) extends Executable with StandardResults {
  /** executing a field does nothing */
  override def execute = success
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T): this.type = {
    value(v)
    this
  }
  /** @return the field value */
  def apply(): T = value.get
  /** alias for apply() */
  def get: T = apply()
  /** @return "label: value" */
  override def toString = label + ": " + this.get
  /** transforms this typed Field as a Field containing the toString value of the Fields value*/
  def toStringField = Field(label, value.get.toString)
}
/**
 * Factory methods for creating Fields. Fields values can also be concatenated to produce "summary" fields.
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

