package org.specs2
package matcher

import language.adhocExtensions
import org.specs2.matcher.describe.Diffable

/**
 * Typed equality Matcher with fallback comparison results
 */
class BeTypedEqualTo[T](t: =>T) extends EqualityMatcher[T](t)(using Diffable.fallbackDiffable)
