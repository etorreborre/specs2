package org.specs2
package guide
package matchers

object ValidationMatchers extends UserGuideCard with matcher.ValidationMatchers {
  def title = "Validation"
  def text = s2"""
  There are several matchers to check `scalaz.Validation` instances:

 * `beSuccess` checks if an element is `Success(_)`
 * `beSuccess(exp)` checks if an element is `Success(exp)
 * `beSuccess(matcher)` checks if an element is `Success(a)` where `a` satisfies the matcher
 * `beSuccess(function: A => AsResult[B])` checks if an element is `Success(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Success` you need to use a matcher: `beSuccess(===(Seq(1))`)
 * `beSuccess.like(partial function)` checks if an element is `Success(_)` and satisfies a partial function returning a `MatchResult`

 * `beFailure` checks if an element is `Failure(_)`
 * `beFailure(exp)` checks if an element is `Failure(exp)`
 * `beFailure(matcher)` checks if an element is `Failure(a)` where `a` satisfies the matcher
 * `beFailure(function: A => AsResult[B])` checks if an element is `Failure(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Failure` you need to use a matcher: `beFailure(===(Seq(1))`)
 * `beFailure.like(partial function)` checks if an element is `Failure(_)` and satisfies a partial function returning a `MatchResult`
  """
}
