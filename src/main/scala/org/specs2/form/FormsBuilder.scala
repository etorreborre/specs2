package org.specs2
package form

import control.Property
import execute._
import matcher._
import scala.xml.NodeSeq

/**
 * Utility methods to build Fields, Props and Forms and insert them in other Forms or 
 * Fragments.
 */
private[specs2]
trait FormsBuilder {

  /** anything can be added on a Form row as a Field */
  implicit def anyAreField[T](t: T) = Field(t)
  /** anything can be added on a Form row as a TextCell */
  implicit def anyAreFieldCell[T](t: T) = fieldsAreTextCell(Field(t))
  /** a Field can be added on a Form row as a FieldCell */
  implicit def fieldsAreTextCell[T](t: Field[T]) = new FieldCell(t)
  /** a Prop can be added on a Form row as a PropCell */
  implicit def propsAreCell(t: Prop[_, _]) = new PropCell(t)
  /** a Form can be added on a Form row as a FormCell */
  implicit def formsAreCell(t: Form) = new FormCell(t)
  implicit def formsAreExecutable(f: Form): Result = f.execute

  /** @return a new Form with the given title */
  def form(title: String) = Form(title)

  /** @return a new Form with the given title and rows */
  def form(title: String, lines: Seq[Form]) = {
    if (lines.isEmpty) Form(title)
    else {
      val header = lines(0).header.map(_.text) match {
        case Nil => Form(title)
        case h :: rest => Form(title).th(h, rest:_*)
      }
      lines.foldLeft(header) { (res, cur) => res.tr(cur) }
    }
  }

  /** @return a new Field with no label and a value */
  def field[T](value: =>T): Field[T] = Field(value)
  /** @return a new Field with a label and a value */
  def field[T](label: String, value: =>T): Field[T] = Field(label, value)
  /** @return a new Field with a label and several values */
  def field(label: String, value1: Field[_], values: Field[_]*): Field[String] = Field(label, value1, values:_*)
  
  /** @return a new Prop with an actual value only */
  def prop[T](value: =>T) = new Prop[T, T](actual = Property(value))
  /** @return a new Prop with a label and an actual value only */
  def prop[T](label: String, actual: =>T) = Prop[T](label, actual)
  /** @return a new Prop with a label, an actual value and expected value */
  def prop[T, S](label: String, actual: =>T, exp: =>S) =
    new Prop[T, S](label, new Property(() => Some(actual)), new Property(() => Some(exp)))
  /** @return a new Prop with a label, an actual value and a constraint to apply to values */
  def prop[T, S](label: String, act: =>T, c: (T, S) => Result) = Prop(label, act, c)
  /** @return a new Prop with a label, an actual value and a matcher to apply to values */
  def prop[T, S](label: String, act: =>T, c: (S) => Matcher[T]) = Prop(label, act, c)
  
}
private[specs2]
object FormsBuilder extends FormsBuilder
