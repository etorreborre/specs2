package org.specs2
package guide
package matchers

object ValidatedMatchers extends UserGuideCard {
  def title = "Validated"
  def text = s2"""
  There are several matchers to check `cats.Validated` instances:

 * `beValid` checks if an element is `Valid(_)`
 * `beValid(exp)` checks if an element is `Valid(exp)`
 * `beValid(matcher)` checks if an element is `Valid(a)` where `a` satisfies the matcher
 * `beValid(function: A => AsResult[B])` checks if an element is `Valid(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Valid` you need to use a matcher: `beValid(===(Seq(1))`)
 * `beValid.like(partial function)` checks if an element is `Valid(_)` and satisfies a partial function returning a `MatchResult`

 * `beInvalid` checks if an element is `Invalid(_)`
 * `beInvalid(exp)` checks if an element is `Invalid(exp)`
 * `beInvalid(matcher)` checks if an element is `Invalid(a)` where `a` satisfies the matcher
 * `beInvalid(function: A => AsResult[B])` checks if an element is `Invalid(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Invalid` you need to use a matcher: `beInvalid(===(Seq(1))`)
 * `beInvalid.like(partial function)` checks if an element is `Invalid(_)` and satisfies a partial function returning a `MatchResult`
  """
}
