package org.specs2
package matcher

import execute._
import text.Regexes._

/**
 * Matchers for checking if a piece of code compiles or not
 */
trait TypecheckMatchers extends TypecheckBeHaveMatchers {
  def succeed: TypecheckMatcher =
    new TypecheckMatcher

  def failWith(message: String): FailTypecheckMatcher =
    FailTypecheckMatcher(message)
}

object TypecheckMatchers extends TypecheckMatchers

trait TypecheckBeHaveMatchers { outer: TypecheckMatchers =>
  implicit class ToTypecheckResultMatcher(result: MatchResult[Typechecked]) {
    def succeed = result(outer.succeed)
  }
}

case class TypecheckMatcher(expectedWarnings: Boolean = false) extends Matcher[Typechecked] {
  def apply[S <: Typechecked](actual: Expectable[S]): MatchResult[S] = {
    println("actual "+actual.value)
    result(actual.value.isSuccess && (!expectedWarnings || actual.value.hasWarnings),
      s"no typecheck error",
      message(actual.value.result), actual)
  }

  def withWarnings: TypecheckMatcher =
    copy(expectedWarnings = true)

  private def message(r: TypecheckResult): String =
    r match {
      case TypecheckSuccess(w)         => "the code typechecks ok"+(if (w) " with warnings" else "")
      case CanTypecheckLiteralsOnly    => "only literals can be typechecked"
      case TypecheckError(m)           => "typecheck error: "+m
      case ParseError(m)               => "parse error: "+m
      case UnexpectedTypecheckError(m) => "unexpected error: "+m
    }
}

case class FailTypecheckMatcher(expected: String) extends Matcher[Typechecked] {
  def apply[S <: Typechecked](actual: Expectable[S]): MatchResult[S] = {
    result(!actual.value.isSuccess && resultMessage(actual.value.result).exists(_ matchesSafely ".*"+expected+".*"),
      s"no compilation error",
      message(actual.value.result, expected), actual)
  }

  private def resultMessage(r: TypecheckResult): Option[String] =
    r match {
      case TypecheckSuccess(_)         => None
      case CanTypecheckLiteralsOnly    => None
      case TypecheckError(m)           => Some(m)
      case ParseError(m)               => Some(m)
      case UnexpectedTypecheckError(m) => Some(m)
    }

  private def message(r: TypecheckResult, expected: String): String =
    r match {
      case TypecheckSuccess(w)         => "the code typechecks ok"+(if (w) " with warnings" else "")
      case CanTypecheckLiteralsOnly    => "only literals can be typechecked"
      case TypecheckError(m)           => s"$m\n doesn't match\n$expected"
      case ParseError(m)               => s"$m\n doesn't match\n$expected"
      case UnexpectedTypecheckError(m) => s"$m\n doesn't match\n$expected"
    }

}

