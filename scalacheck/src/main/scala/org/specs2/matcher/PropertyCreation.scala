package org.specs2
package matcher

import org.scalacheck.{Gen, Shrink, Arbitrary, Prop}
import execute._

import scalaz.{Reducer, Show}

trait PropertyCreation {

  /**
   * transform a Function returning a MatchResult (or anything which can be converted to a Prop) as a ScalaCheck property
   */
  def prop[T, R](result: T => R)(implicit arbitrary: Arbitrary[T], shrink: Shrink[T],
                                    show: Show[T], asResult: AsResult[R], collector: Reducer[T, String],
                                    parameters: Parameters): ScalaCheckProperty[T, R] =
    ScalaCheckProperty(result, arbitrary, Some(shrink), asResult, show, collector, context = None, parameters)

}

