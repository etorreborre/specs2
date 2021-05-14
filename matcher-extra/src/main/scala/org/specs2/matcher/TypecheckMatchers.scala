package org.specs2
package matcher

import execute.*, Result.*
import text.Regexes.*
import text.Trim.*
import StringMatchers.*

/**
 * Matchers for checking if a piece of code compiles or not
 */
trait TypecheckMatchers:
  def succeed: Matcher[TypecheckResult] =
    TypecheckMatcher()

  def failWith(message: String): Matcher[TypecheckResult] =
    FailTypecheckMatcher(message)

object TypecheckMatchers extends TypecheckMatchers

class TypecheckMatcher extends Matcher[TypecheckResult]:
  def apply[S <: TypecheckResult](actual: Expectable[S]): Result =
    AsResult(actual.value: TypecheckResult)

case class FailTypecheckMatcher(expected: String) extends Matcher[TypecheckResult]:
  def apply[S <: TypecheckResult](actual: Expectable[S]): Result =
    val r = AsResult(actual.value: TypecheckResult)
    val matchOk = beMatchingWithPart(expected).apply(createExpectable(r.message)).isSuccess
    result(!r.isSuccess && matchOk,
      if r.isSuccess
        then s"Expected a Failure, got ${r.getClass}"
        else s"${r.message}\n doesn't match\n$expected")
