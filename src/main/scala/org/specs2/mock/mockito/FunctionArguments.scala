package org.specs2
package mock
package mockito

import matcher.{BeEqualTo, Matcher}
import org.hamcrest.core.IsAnything


/**
 * This trait allows to specify functions as arguments in mocked methods.
 *
 * It uses a pair of (arguments, expected result) to specify what was the passed function:
 *
 *       function2.call((i:Int, d: Double) => (i + d).toString)
 *       there was one(function2).call((1, 3.0) -> "4.0")
 *
 * anyFunctionN will just match any function with n arguments
 *
 */
trait FunctionArguments extends ArgThat {

  implicit def callMatching[A, R](a: A, m: Matcher[R]): A => R = argThat(m ^^ { (f: A => R) => f(a) })
  implicit def functionCall[A, R](a: A, r: R): A => R = callMatching(a, new BeEqualTo(r))

  implicit def toFunctionCall[A, R](values: (A, R)): A => R = functionCall(values._1, values._2)
  implicit def matcherToFunctionCall[A, R](values: (A, Matcher[R])): A => R = callMatching(values._1, values._2)

  implicit def callMatching2[A, B, R](a: A, b: B,  m: Matcher[R]): Function2[A, B, R] = argThat(m ^^ { (f: Function2[A, B, R]) => f(a, b) })
  implicit def functionCall2[A, B, R](a: A, b: B, r: R): Function2[A, B, R] = callMatching2(a, b, new BeEqualTo(r))

  implicit def toFunctionCall2[A, B, R](values: ((A, B), R)): Function2[A, B, R] = functionCall2(values._1._1, values._1._2, values._2)
  implicit def matcherToFunctionCall2[A, B, R](values: ((A, B), Matcher[R])): Function2[A, B, R] = callMatching2(values._1._1, values._1._2, values._2)

}
