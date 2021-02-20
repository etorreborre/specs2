package org.specs2
package matcher

import execute.{Result, StandardResults}
import scala.util.NotGiven

/**
 * This trait provides implicit definitions to check values with matchers
 * by using a "should" syntax: value should matcher
 */
trait ShouldExpectations extends ExpectationsCreation with TypedEqual:

  implicit class expectShould[T](tm: =>T)(using not: NotGiven[NoShouldExpectations]):
    def should(m: =>Matcher[T]) =
      createExpectable(tm).applyMatcher(m)

  implicit class expectedShould [T](tm: Expectable[T])(using not: NotGiven[NoShouldExpectations]):
    def should(m: =>Matcher[T]) =
      tm.applyMatcher(m)

object ShouldExpectations extends ShouldExpectations

trait NoShouldExpectations:
  given NoShouldExpectations = ???

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 * which throws exceptions when a match fails
 */
trait ShouldThrownExpectations extends ShouldExpectations with ThrownExpectations

object ShouldThrownExpectations extends ShouldThrownExpectations
