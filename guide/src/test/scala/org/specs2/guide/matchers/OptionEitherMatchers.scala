package org.specs2
package guide
package matchers

object OptionEitherMatchers extends UserGuideCard {
  def title = "Option/Either"
  def text = s2"""
  There are several matchers to check `Option` and `Either` instances:

 * `beSome` checks if an element is `Some(_)`
 * `beSome(exp)` checks if an element is `Some(exp)`
 * `beSome(matcher)` checks if an element is `Some(a)` where `a` satisfies the matcher
 * `beSome(function: A => AsResult[B])` checks if an element is `Some(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Some` you need to use a matcher: `beSome(===(Seq(1))`)
 * `beSome.which(function)` checks if an element is `Some(_`) and satisfies a function returning a boolean
 * `beSome.like(partial function)` checks if an element is `Some(_)` and satisfies a partial function returning a `Result`
 * `beNone` checks if an element is `None`
 * `beAsNoneAs` checks if 2 values are equal to `None` at the same time

 * `beRight` checks if an element is `Right(_)`
 * `beRight(exp)` checks if an element is `Right(exp)
 * `beRight(matcher)` checks if an element is `Right(a)` where `a` satisfies the matcher
 * `beRight(function: A => AsResult[B])` checks if an element is `Right(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Right` you need to use a matcher: `beRight(===(Seq(1))`)
 * `beRight.like(partial function)` checks if an element is `Right(_)` and satisfies a partial function returning a `Result`

 * `beLeft` checks if an element is `Left(_)`
 * `beLeft(exp)` checks if an element is `Left(exp)`
 * `beLeft(matcher)` checks if an element is `Left(a)` where `a` satisfies the matcher
 * `beLeft(function: A => AsResult[B])` checks if an element is `Left(a)` where `function(a)` returns a successful `Result`
    (note that a `Seq[A]` is also a function `Int => A` so if you want to check that a sequence is contained in `Left` you need to use a matcher: `beLeft(===(Seq(1))`)
 * `beLeft.like(partial function)` checks if an element is `Left(_)` and satisfies a partial function returning a `Result`
  """
}
