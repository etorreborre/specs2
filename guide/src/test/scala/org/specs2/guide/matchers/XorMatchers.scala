package org.specs2
package guide
package matchers

object XorMatchers extends UserGuideCard with matcher.DisjunctionMatchers {
  def title = "Disjunction"
  def text = s2"""
  There are several matchers to check `cats.Xor` instances:

 * `beXorRight` checks if an element is `Right(_)`
 * `beXorRight(exp)` checks if an element is `Right(exp)
 * `beXorRight(matcher)` checks if an element is `Right(a)` where `a` satisfies the matcher
 * `beXorRight(function: A => AsResult[B])` checks if an element is `Right(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Right` you need to use a matcher: `beXorRight(===(Seq(1))`)
 * `beXorRight.like(partial function)` checks if an element is `Right(_)` and satisfies a partial function returning a `MatchResult`

 * `beXorLeft` checks if an element is `Left(_)`
 * `beXorLeft(exp)` checks if an element is `Left(exp)`
 * `beXorLeft(matcher)` checks if an element is `Left(a)` where `a` satisfies the matcher
 * `beXorLeft(function: A => AsResult[B])` checks if an element is `Left(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Left` you need to use a matcher: `beXorLeft(===(Seq(1))`)
 * `beXorLeft.like(partial function)` checks if an element is `Left(_)` and satisfies a partial function returning a `MatchResult`
  """
}

