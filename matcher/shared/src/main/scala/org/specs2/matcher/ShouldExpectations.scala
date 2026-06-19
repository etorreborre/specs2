package org.specs2
package matcher

import execute.{Result, StandardResults}
import scala.util.NotGiven

/** This trait provides implicit definitions to check values with matchers by using a "should" syntax: value should
  * matcher
  */
trait ShouldExpectations extends ExpectationsCreation with TypedEqual:

  implicit class expectShould[T](tm: =>T)(using not: NotGiven[NoShouldExpectations]):
    infix def should(m: =>Matcher[T]) =
      createExpectable(tm).applyMatcher(m)

  /** This extension is necessary to disambiguate the `should` used to check a String value (`"a" should beEqualTo("a")`)
    * from the `should` used to create a block of examples in a mutable specification (`"a" should { ... }`). Without it
    * the block DSL `should` extension method defined on `String` shadows the `expectShould` implicit class above.
    */
  extension (s: =>String)(using not: NotGiven[NoShouldExpectations])
    infix def should(m: =>Matcher[String]) =
      createExpectable(s).applyMatcher(m)

  implicit class expectedShould[T](tm: Expectable[T])(using not: NotGiven[NoShouldExpectations]):
    infix def should(m: =>Matcher[T]) =
      tm.applyMatcher(m)

object ShouldExpectations extends ShouldExpectations

trait NoShouldExpectations:
  given NoShouldExpectations = ???

/** This trait provides implicit definitions to transform any value into an Expectable which throws exceptions when a
  * match fails
  */
trait ShouldThrownExpectations extends ShouldExpectations with ThrownExpectations

object ShouldThrownExpectations extends ShouldThrownExpectations
