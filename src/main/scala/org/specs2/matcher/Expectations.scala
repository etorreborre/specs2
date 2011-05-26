package org.specs2
package matcher

import execute._
import AnyMatchers._
import junit.framework.{ ComparisonFailure, AssertionFailedError }

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 */
trait Expectations {
  /** describe a value with the aka method */
  implicit def describe[T](t: =>T): Descriptible[T] = new Descriptible(t)
  class Descriptible[T](value: =>T) {
    /**
     * @return an expectable with its toString method as an alias description
     *         this is useful to preserve the original value when the matcher using
     *         it is adapting the value
     */
    def aka: Expectable[T] = aka(value.toString)
    /** @return an expectable with an alias description */
    def aka(alias: String): Expectable[T] = createExpectable(value, alias)
    /** @return an expectable with an alias description, after the value string */
    def post(alias: String): Expectable[T] = as((_:String) + alias)
    /** @return an expectable with an alias description, after the value string */
    def as(alias: String => String): Expectable[T] = createExpectable(value, alias)
  }

  /**
   * A value can be tested against another with the === operator.
   * It is equivalent to writting a must_== b
   */
  implicit def canBeEqual[T](t: =>T) = new CanBeEqual(t)
  class CanBeEqual[T](t: =>T) {
    /** equality matcher on Expectables */
    def ===[S >: T](other: =>S) = createExpectable(t).applyMatcher(new BeEqualTo(other))
    /** ! equality matcher on Expectables */
    def !==[S >: T](other: =>S) = createExpectable(t).applyMatcher(new BeEqualTo(other).not)
    /** typed equality matcher on Expectables */
    def ====[S >: T](other: =>S) = createExpectable(t).applyMatcher(new BeTypedEqualTo(other))
    /** ! typed equality matcher on Expectables */
    def !===[S >: T](other: =>S) = createExpectable(t).applyMatcher(new BeTypedEqualTo(other).not)
  }

  /** @return an Expectable */
  def createExpectable[T](t: =>T): Expectable[T] = createExpectable(t, None)
  /** @return an Expectable with a description */
  def createExpectable[T](t: =>T, alias: String): Expectable[T] = createExpectable(t, Some(Expectable.aliasDisplay(alias)))
  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: String => String): Expectable[T] = createExpectable(t, Some(alias))
  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] = Expectable(t, alias)
  
}
