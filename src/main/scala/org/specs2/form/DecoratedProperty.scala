package org.specs2
package form

/**
 * A DecoratedLabel holds a decorator and delegates decoration and styling operations for values and labels to that Decorator
 */
trait DecoratedProperty[T] extends DecoratedLabel[T] {
  /** set a new Decorator */
  def decorateWith(f: Any => Any) = decoratorIs(decorator.decorateWith(f))
  /** set a new Decorator for the value */
  def decorateValueWith(f: Any => Any) = decoratorIs(decorator.decorateValueWith(f))
  /** set a new style */
  def styleWith(s: (String, String)) = decoratorIs(decorator.styleWith(s))
  /** set a new style for the value */
  def styleValueWith(s: (String, String)) = decoratorIs(decorator.styleValueWith(s))
  /** do the decoration */
  def decorateValue(ns: Any) = decorator.value(ns)

  def valueStyles = decorator.valueStyles.mkString("; ")
}
