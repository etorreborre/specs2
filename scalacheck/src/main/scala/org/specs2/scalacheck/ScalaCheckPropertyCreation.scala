package org.specs2
package scalacheck

import org.scalacheck._
import org.scalacheck.util.Pretty
import execute.AsResult

trait ScalaCheckPropertyCreation {

  /** create a ScalaCheck property from a function */
  def prop[T, R](result: T => R)(
    implicit arbitrary: Arbitrary[T], shrink: Shrink[T], pretty: T => Pretty,
             asResult: AsResult[R], parameters: Parameters): ScalaCheckFunction1[T, R] =
    ScalaCheckFunction1(result, arbitrary, Some(shrink), pretty, collector = None, asResult, context = None, parameters)

  /** create a ScalaCheck property from a function of 2 arguments */
  def prop[T1, T2, R](result: (T1, T2) => R)(implicit
    arbitrary1: Arbitrary[T1], shrink1: Shrink[T1],
    arbitrary2: Arbitrary[T2], shrink2: Shrink[T2],
    asResult: AsResult[R],
    pretty1: T1 => Pretty,
    pretty2: T2 => Pretty,
    parameters: Parameters): ScalaCheckFunction2[T1, T2, R] =
    ScalaCheckFunction2(result,
      arbitrary1, arbitrary2,
      Some(shrink1), Some(shrink2),
      pretty1, pretty2,
      collector1 = None, collector2 = None,
      asResult,
      context = None, parameters)

  /** create a ScalaCheck property from a function of 3 arguments */
  def prop[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit
                                             arbitrary1: Arbitrary[T1], shrink1: Shrink[T1],
                                             arbitrary2: Arbitrary[T2], shrink2: Shrink[T2],
                                             arbitrary3: Arbitrary[T3], shrink3: Shrink[T3],
                                             asResult: AsResult[R],
                                             pretty1: T1 => Pretty,
                                             pretty2: T2 => Pretty,
                                             pretty3: T3 => Pretty,
                                             parameters: Parameters): ScalaCheckFunction3[T1, T2, T3, R] =
    ScalaCheckFunction3(result,
      arbitrary1, arbitrary2, arbitrary3,
      Some(shrink1), Some(shrink2), Some(shrink3),
      pretty1, pretty2, pretty3,
      collector1 = None, collector2 = None, collector3 = None,
      asResult,
      context = None, parameters)

}

