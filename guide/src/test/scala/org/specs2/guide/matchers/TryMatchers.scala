package org.specs2
package guide
package matchers

object TryMatchers extends UserGuideCard {
  def title = "Try"
  def text = s2"""
  There are several matchers to check `Try` instances:

 * `beSuccessfulTry` check if an element is `Success(_)`
 * `beSuccessfulTry.withValue(exp)` check if an element is `Success(_)`
 * `beSuccessfulTry.withValue(matcher)` check if an element is `Success(a)` where `a` satisfies the matcher
 * `beSuccessfulTry.withValue(function: A => AsResult[B])` check if an element is `Success(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Success` you need to use a matcher: `beSuccessfulTry.withValue(===(Seq(1))`)
 * `beSuccessfulTry.which(function)` check if an element is `Success(_)` and satisfies a function returning a boolean
 * `beSuccessfulTry.like(partial function)` check if an element is `Success(_)` and satisfies a partial function returning a `MatchResult`
 * `beFailedTry` check if an element is `Failure(_)`
 * `beFailedTry.withThrowable[T]` check if an element is `Failure(t: T)`
 * `beFailedTry.withThrowable[T](pattern)` check if an element is `Failure(t: T)` and `t.getMessage` matches `pattern`
  """
}
