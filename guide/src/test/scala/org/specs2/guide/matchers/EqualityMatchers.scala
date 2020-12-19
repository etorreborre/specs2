package org.specs2
package guide
package matchers

object EqualityMatchers extends UserGuideCard {
  def title = "Equality"
  def text = s2"""
The most common type of matcher is `beEqualTo` to test the equality of 2 values with the underlying `==` operator. Several syntaxes can be used, according to your own taste

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a symbol
 `1 should ==(1)           `| for `should` lovers
 `1 === 1                  `| the ultimate shortcut

There are also other notions of equality

 Matcher            |  Comment
 ----------         | -----------------------------------------------------
 `beTypedEqualTo   `| typed equality. `a must beTypedEqualTo(b)` will not work if `a` and `b` don't have compatible types
 `be_===           `| synonym for `beTypedEqualTo`
 `a ==== b         `| synonym for `a must beTypedEqualTo(b)`
 `a must ====(b)   `| similar to `a must ==(b)` but will not typecheck if `a` and `b` don't have the same type
 `be_==~           `| check if `(a: A) == conv(b: B)` when there is an implicit conversion `conv` from `B` to `A`
 `beTheSameAs      `| reference equality: check if `a eq b` (`a must be(b)` also works)
 `be               `| `a must be(b)`: synonym for `beTheSameAs`
 `beTrue, beFalse  `| shortcuts for Boolean equality
 `beLike           `| partial equality, using a `PartialFunction[T, Result]`: `(1, 2) must beLike { case (1, _) => ok }`

_Note_: the `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`.
So the `beEqualTo` matcher has been adapted to use the `.deep` method on `Arrays`, transforming them to `IndexedSeqs` (possibly nested),
before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).

"""

}

case class Human(age: Int, wealth: Int)
