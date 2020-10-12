package org.specs2
package matcher

import execute.{Result, StandardResults}
import scala.implicits.Not

/**
 * This trait provides implicit definitions to check values with matchers
 * by using a "must" syntax: value must matcher
 */
trait MustExpectations extends ExpectationsCreation with TypedEqual:

  extension [T](tm: =>T)(using not: Not[NoMustExpectations])
    def must(m: =>Matcher[T]) =
      createExpectable(tm).applyMatcher(m)

  extension [T](tm: Expectable[T])(using not: Not[NoMustExpectations])
    def must(m: =>Matcher[T]) =
      tm.applyMatcher(m)

object MustExpectations extends MustExpectations

trait NoMustExpectations:
  given NoMustExpectations = ???

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 * which throws exceptions when a match fails
 */
trait MustThrownExpectations extends MustExpectations with ThrownExpectations

object MustThrownExpectations extends MustThrownExpectations
