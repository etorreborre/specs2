package org.specs2
package guide
package matchers

object EqualityMatchers extends UserGuideCard {
  def title = "Equality"
  def text = s2"""
The most common type of matcher is `beEqualTo` to test the equality of 2 values with the underlying `==` operator where:

 - the compared types must be the same (as if the `language:strictEquality` option has been turned on)

 - the comparison of `Arrays` uses the `.deep` method on `Arrays`, transforming them to `IndexedSeqs` (possibly nested)
   Otherwise `==` on arrays uses the reference equality, so that `Array(1, 2, 3) === Array(1, 2, 3)`, despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`

Several syntaxes can be used, according to your own taste

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a symbol
 `1 should be_==(1)        `| for should lovers
 `1 === 1                  `| the ultimate shortcut

There are also other notions of equality

 Matcher            |  Comment
 ----------         | -----------------------------------------------------
 `be_==~           `| check if `(a: A) === conversion(b: B)` when there is an implicit conversion `Conversion[B, A]`
 `beTheSameAs      `| reference equality: check if `a eq b` (`a must be(b)` also works)
 `be               `| `a must be(b)`: synonym for `beTheSameAs`
 `beTrue, beFalse  `| shortcuts for Boolean equality
 `beLike           `| partial equality, using a `PartialFunction[T, Result]`: `(1, 2) must beLike { case (1, _) => ok }`

"""

}

case class Human(age: Int, wealth: Int)
