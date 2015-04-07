package org.specs2
package matcher

import execute._
import text.Regexes._

/**
 * Matchers for checking if a piece of code compiles or not
 */
trait TypecheckMatchers extends TypecheckBeHaveMatchers {
  def succeed: Matcher[Typechecked] =
    new TypecheckMatcher

  def failWith(message: String): Matcher[Typechecked] =
    FailTypecheckMatcher(message)
}

trait TypecheckBeHaveMatchers { outer: TypecheckMatchers =>
  implicit class ToTypecheckResultMatcher(result: MatchResult[Typechecked]) {
    def succeed = result(outer.succeed)
  }
}

class TypecheckMatcher extends Matcher[Typechecked] {
  def apply[S <: Typechecked](actual: Expectable[S]): MatchResult[S] = {
    result(actual.value.isSuccess,
      s"no typecheck error",
      message(actual.value.result), actual)
  }

  private def message(r: TypecheckResult): String =
    r match {
      case TypecheckSuccess            => "typecheck error"
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
      case TypecheckSuccess            => None
      case CanTypecheckLiteralsOnly    => None
      case TypecheckError(m)           => Some(m)
      case ParseError(m)               => Some(m)
      case UnexpectedTypecheckError(m) => Some(m)
    }

  private def message(r: TypecheckResult, expected: String): String =
    r match {
      case TypecheckSuccess            => "the code typechecks ok"
      case CanTypecheckLiteralsOnly    => "only literals can be typechecked"
      case TypecheckError(m)           => s"$m\n doesn't match\n$expected"
      case ParseError(m)               => s"$m\n doesn't match\n$expected"
      case UnexpectedTypecheckError(m) => s"$m\n doesn't match\n$expected"
    }

}

