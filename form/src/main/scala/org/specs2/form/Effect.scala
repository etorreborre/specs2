package org.specs2
package form

import control.Exceptions._
import control.Property
import execute._
import DecoratedProperties._
import StandardResults._

/**
 * An Effect is a property which is used to display names corresponding to side-effects.
 *
 * If the side effect throws an exception, the Effect will display it alongside to the label. Otherwise only the label
 * is displayed.
 * 
 * The apply method can be used to execute the Effect effect and possibly get a value out of it (but usually not displayed):
 *   `Effect(label, 1).apply() must_== 1`
 * 
 * The value is stored in a Property object so it will not be evaluated until explicitly queried.
 */
case class Effect[T](label: String, value: Property[T], decorator: Decorator = Decorator()) extends Executable with DecoratedProperty[Effect[T]] {
  /** executing an effect executes the value and returns success unless there is an Error */
  override def execute = {
    valueOrResult match {
      case Left(e)  => e
      case Right(v) => success
    }
  }

  def valueOrResult: Either[Result, T] =
    value.toRight[Result](skipped).fold(r => Left(r), v => trye(v)(Error(_)))

  /** set a new value on the effect */
  def apply(v: =>T) = new Effect(label, value(v), decorator)

  def map[S](f: T => S) = new Effect(label, value.map(f), decorator)
  def flatMap[S](f: T => Option[S]) = new Effect(label, value.flatMap(f), decorator)

  /** set a new Decorator */
  def decoratorIs(d: Decorator) = copy(decorator = d)

  /** use this Effect as a header in a table */
  def header = this.center.bold.bkGrey

  override def toString = label

  override def equals(a: Any) = a match {
    case Effect(l, v, _) => label == l && value == v
    case other           => false
  }

  override def hashCode = label.hashCode + value.hashCode
}
/**
 * Factory methods for creating Effects. Effects values can also be concatenated to produce
 * "summary" effects.
 * 
 * val e1 = Effect("hello", print("hello"))
 * val e2 = Effect("world", print("world"))
 * val concatenatedEffects = Effect(e1, e2)
 * concatenatedEffects.toString == hello/world
 * 
 * val concatenatedEffect = Effect(", ", e1, e2)
 * concatenatedEffects2.toString == hello, world
 */
case object Effect {
  /** create an Effect from a value */
  def apply[T](value: =>T): Effect[T] = new Effect("", Property(value))

  /** create an Effect with a label and a value */
  def apply[T](label: String, value: =>T): Effect[T] = new Effect(label, Property(value))

  /** create an Effect from several other ones concatenating the labels */
  def apply(e1: Effect[_], es: Effect[_]*): Effect[Any] = Effect("/", e1, es:_*)

  /** create an Effect from several other ones concatenating the labels */
  def apply(separator: String, e1: Effect[_], es: Effect[_]*): Effect[Any] =
    Effect((e1 :: es.toList).map(_.label).mkString(separator), (e1 :: es.toList).foreach((e: Effect[_]) => e.execute))
}
