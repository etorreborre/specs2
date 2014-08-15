package org.specs2
package guide
package matchers

object DisjunctionMatchers extends UserGuideCard with matcher.DisjunctionMatchers {
  def title = "Disjunction"
  def text = s2"""
  There are several matchers to check `scalaz.\/` instances:

 * `be_\/-` checks if an element is `\/-(_)`
 * `be_\/-(exp)` checks if an element is `\/-(exp)
 * `be_\/-(matcher)` checks if an element is `\/-(a)` where `a` satisfies the matcher
 * `be_\/-(function: A => AsResult[B])` checks if an element is `\/-(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `\/-` you need to use a matcher: `be_\/-(===(Seq(1))`)
 * `be_\/-.like(partial function)` checks if an element is `\/-(_)` and satisfies a partial function returning a `MatchResult`

 * `be_-\/` checks if an element is `-\/(_)`
 * `be_-\/(exp)` checks if an element is `-\/(exp)`
 * `be_-\/(matcher)` checks if an element is `-\/(a)` where `a` satisfies the matcher
 * `be_-\/(function: A => AsResult[B])` checks if an element is `-\/(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `-\/` you need to use a matcher: `be_-\/(===(Seq(1))`)
 * `be_-\/.like(partial function)` checks if an element is `-\/(_)` and satisfies a partial function returning a `MatchResult`
  """
}

