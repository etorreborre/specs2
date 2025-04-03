package org.specs2
package matcher

import org.specs2.matcher.describe.Diffable
import scala.util.NotGiven
import execute.*

/** This trait adds some implicits to create expectations with the `===` sign
  */
trait TypedEqual:
  this: ExpectationsCreation =>

  /** A value can be tested against another with the === operator. It is equivalent to writing a must ==(b)
    */
  extension [T: Diffable](t: =>T)(using NotGiven[NoTypedEqual], NotGiven[T =:= Any], NotGiven[T =:= AnyRef])
    /** equality matcher on Expectables */
    def ===(other: =>T): Result =
      createExpectable(t).applyMatcher[T](new EqualityMatcher(other))

    /** ! equality matcher on Expectables */
    def !==(other: =>T): Result =
      createExpectable(t).applyMatcher[T](new EqualityMatcher(other).not)

object TypedEqual extends TypedEqual with ExpectationsCreation

/** This trait can be used to suppress the TypedEqual implicit
  */
trait NoTypedEqual extends TypedEqual:
  self: ExpectationsCreation =>
  given NoTypedEqual = ???
