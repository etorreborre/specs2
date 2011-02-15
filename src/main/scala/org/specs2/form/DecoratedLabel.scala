package org.specs2
package form

/**
 * A DecoratedLabel holds a decorator and delegates decoration and styling operations to that Decorator
 */
trait DecoratedLabel[T] {
  val decorator: Decorator
  /** set a new Decorator */
  def decoratorIs(d: Decorator): T
  /** set a new Decorator for the label */
  def decorateLabelWith(f: Any => Any) = decoratorIs(decorator.decorateLabelWith(f))
  /** set a new style for the label */
  def styleLabelWith(s: (String, String)) = decoratorIs(decorator.styleLabelWith(s))
  /** do the decoration */
  def decorateLabel(ns: Any) = decorator.label(ns)
  /** return the label styles */
  def labelStyles = decorator.labelStyles.mkString("; ")
}
