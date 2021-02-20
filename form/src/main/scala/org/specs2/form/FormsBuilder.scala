package org.specs2
package form

import control.Property
import control.Properties.*
import execute.*
import matcher.*

import scala.xml.NodeSeq

/**
 * Utility methods to build Fields, Props and Forms and insert them in other Forms or
 * Fragments.
 */
private[specs2]
trait FormsBuilder extends FormsBuilderLowPriorityImplicits:

  /** a Field can be added on a Form row as a FieldCell */
  given [T]: Conversion[Field[T], FieldCell] with
    def apply(t: Field[T]): FieldCell =
      new FieldCell(t)

  /** a Effect can be added on a Form row as a EffectCell */
  given Conversion[Effect[?], EffectCell] with
    def apply(t: Effect[?]): EffectCell =
      new EffectCell(t)

  /** a Prop can be added on a Form row as a PropCell */
  given [T, S]: Conversion[Prop[T, S], PropCell] with
    def apply(t: Prop[T, S]): PropCell =
      new PropCell(t)

  /** a Form can be implicitly executed if necessary */
  given Conversion[Form, Result] with
    def apply(f: Form): Result = f.execute

  /** any seq of object convertible to cells */
  given [T : ToCell]: Conversion[Seq[T], Seq[Cell]] with
    def apply(seq: Seq[T]): Seq[Cell] =
      seq.map(summon[ToCell[T]].toCell)

  /** a cell can be added lazily to a row. It will only be evaluated when necessary */
  def lazify(c: =>Cell): LazyCell =
    new LazyCell(c)

  /** @return a new Form with the given title */
  def form(title: String): Form =
    Form(title)

  /** @return a new Field with no label and a value */
  def field[T](value: =>T): Field[T] =
    Field(value)

  /** @return a new Field with a label and a value */
  def field[T](label: String, value: =>T): Field[T] =
    Field(label, value)

  /** @return a new Effect with a label and a value */
  def effect[T](label: String, value: =>T): Effect[T] =
    Effect(label, value)

  /** @return a new Field with a label and several values */
  def field(label: String, value1: Field[?], values: Field[?]*): Field[String] =
    Field(label, value1, values*)

  /** @return a new Prop with an actual value only */
  def prop[T](act: =>T): Prop[T, T] =
    new Prop[T, T](actual = Property(act))

  /** @return a new Prop with a label and an actual value only */
  def prop[T](label: String, actual: =>T): Prop[T, T] =
    Prop[T](label, actual)

  /** @return a new Prop with a label, an actual value and expected value */
  def prop[T, S](label: String, actual: =>T, exp: =>S): Prop[T, S] =
    new Prop[T, S](label, new Property(() => Some(actual)), new Property(() => Some(exp)))

  /** @return a new Prop with a label, an actual value and a constraint to apply to values */
  def prop[T, S, R : AsResult](label: String, actual: =>T, c: (T, S) => R): Prop[T, S] =
    Prop(label, actual, c)

  /** @return a new Prop with a label, an actual value and a matcher to apply to values */
  def prop[T, S](label: String, actual: =>T, c: (S) => Matcher[T]): Prop[T, S] =
    Prop[T, S](label, actual, c)

  /** @return a new Prop with a label, an actual value and a matcher to apply to the actual value */
  def prop[T](label: String, actual: =>T, c: Matcher[T]): Prop[T, T] =
    Prop[T](label, actual, c)

  /** @return a new Prop with no label, an actual value and a matcher to apply to the actual value */
  def prop[T](actual: =>T, c: Matcher[T]): Prop[T, T] =
    Prop[T]("", actual, c)

  /** @return a new Prop with a label, an actual value and a matcher to apply to the actual value */
  def prop[T, S](label: String, actual: =>T, expected: =>S, c: Matcher[T]): Prop[T, S] =
    Prop[T, S](label, actual, expected, c)

  /** @return a new Prop with a label, which has the same actual and expected value to test the result of an action */
  def action[T](label: String, a: =>T): Prop[T, T] =
    lazy val act = a
    prop(label, act)(act)

  /** @return a new Tabs object */
  def tabs: Tabs = new Tabs()

  /** @return a new Tabs object with a first tab */
  def tab(label: String, form: Form): Tabs =
    tabs.tab(label, form)

private[specs2]
object FormsBuilder extends FormsBuilder

trait FormsBuilderLowPriorityImplicits:
  /** anything can be added on a Form row as a Field */
  implicit def anyIsField[T](t: =>T): Field[T] =
    Field(t)

  /** anything can be added on a Form row as a TextCell */
  implicit def anyIsFieldCell(t: =>Any): FieldCell =
    FieldCell(Field(t))

  /** any xml can be injected as a cell */
  implicit def xmlIsACell(xml: =>NodeSeq): XmlCell =
    new XmlCell(xml)

  /** a Form can be added on a Form row as a FormCell */
  implicit def formIsCell(t: =>Form): FormCell =
    new FormCell(t)
