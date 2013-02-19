package org.specs2
package matcher

import org.hamcrest._
import MatchersImplicits._

/**
 * This trait adds implicit conversions to be able to use [Hamcrest matchers](http://code.google.com/p/hamcrest) as specs2 matchers.
 */
trait Hamcrest {

  /** convert a Hamcrest matcher to a specs2 matcher */
  implicit def asSpecs2Matcher[T](hamcrest: org.hamcrest.Matcher[T]): matcher.Matcher[T] =
    (t: T) => (hamcrest.matches(t), createKoMessageFromHamcrest(t, hamcrest))

  /**
   * @return a string showing the matched value and the failure message from the Hamcrest matcher
   */
  def createKoMessageFromHamcrest[T](t: =>T, hamcrest: org.hamcrest.Matcher[T]): String = {
    val description = new StringDescription

    description.appendValue(t)
    hamcrest.describeTo(description)
    description.toString
  }

}

object Hamcrest extends Hamcrest
