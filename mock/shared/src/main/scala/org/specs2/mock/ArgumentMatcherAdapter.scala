package org.specs2.mock

import org.mockito.ArgumentMatcher
import org.specs2.matcher.{Expectable, Matcher}

case class ArgumentMatcherAdapter[T](m: Matcher[T]) extends ArgumentMatcher[T] {
  def matches(argument: T): Boolean =
    m.apply(Expectable(argument)).isSuccess
}
