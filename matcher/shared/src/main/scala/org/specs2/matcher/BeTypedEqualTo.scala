package org.specs2
package matcher

import org.specs2.matcher.describe.Diffable

/**
 * Typed equality Matcher with fallback comparison results
 */
class BeTypedEqualTo[T](t: =>T) extends EqualityMatcher[T](t)(Diffable.fallbackDiffable)
