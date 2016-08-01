package org.specs2
package matcher

import org.specs2.matcher.describe.Diffable

/**
 * Typed equality Matcher with fallback comparison results
 */
class BeTypedEqualTo[T](t: =>T, equality: (T, T) => Boolean = (t1:T, t2:T) => t1 == t2) extends
  EqualityMatcher[T](t, equality)(Diffable.fallbackDiffable)
