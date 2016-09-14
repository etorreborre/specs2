package org.specs2
package matcher

import org.specs2.matcher.describe.Diffable

/**
 * This kind of expectable can be followed by the verb must to apply a matcher:
 * 
 * `1 must beEqualTo(1)`
 * 
 * For convenience, several mustMatcher methods have also been defined as shortcuts to equivalent:
 * 
 * `a must matcher`
 */
class MustExpectable[T] private[specs2] (tm: () => T) extends Expectable[T](tm) { outer =>
  def must(m: =>Matcher[T])                                       = applyMatcher(m)
  def mustEqual(other: =>Any)                                     = applyMatcher[Any](new BeEqualTo(other))
  def mustNotEqual(other: =>Any)                                  = applyMatcher[Any](new BeEqualTo(other).not)
  def must_==(other: =>Any)                                       = applyMatcher[Any](new BeEqualTo(other))
  def must_===(other: =>T)(implicit di: Diffable[T]) = applyMatcher(new BeTypedEqualTo2(other))
  def must_!==(other: =>T)(implicit di: Diffable[T]) = applyMatcher(new BeTypedEqualTo2(other).not)
  def must_!=(other: =>Any)                                       = applyMatcher[Any](new BeEqualTo(other).not)
}

object MustExpectable {
  def apply[T](t: =>T) = new MustExpectable(() => t)
}

