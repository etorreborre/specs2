package org.specs2
package matcher

import org.specs2.fp.syntax._

/**
 * Inherit this trait to provide a Matcher where both the actual and the expected values can be adapted with a function.
 */
trait AdaptableMatcher[T] extends Matcher[T] { outer =>
  /**
   * @return a matcher changing its expected value and possibly adding more information to the ok and ko messages
   */
  def adapt(f: T => T, ok: String => String = identity, ko: String => String = identity): AdaptableMatcher[T]
  /**
   * Adapt a matcher with both the expected and actual values
   * ex: `be_==("message") ^^^ (_.trim)` will do the comparison on both trimmed strings
   */
  def ^^^(f: T => T, ok: String => String = identity, ko: String => String = identity): AdaptableMatcher[T] =
    new AdaptableMatcher[T] {
      def adapt(g: T => T, okFunction: String => String, koFunction: String => String): AdaptableMatcher[T] =
        outer.adapt(g compose f, okFunction compose ok, koFunction compose ko)

      def apply[U <: T](a: Expectable[U]) = {
        val result = outer.adapt(f, ok, ko).apply(a.map(f))
        result.map((t: T) => a.value)
      }
    }
  /**
   * Adapts a matcher by changing the actual value before doing the match
   * ex: `be_==("message") ^^ (_.trim)` will do the comparison on a trimmed String before match
   */
  def ^^(f: T => T) = new AdaptableMatcher[T] {
    def adapt(f2: T => T, ok: String => String = identity, ko: String => String = identity) =
      outer.adapt(f2, ok, ko)

    def apply[U <: T](a: Expectable[U]) = {
      val result = outer.apply(a.map(f))
      result.map((t: T) => a.value)
    }
  }
}
