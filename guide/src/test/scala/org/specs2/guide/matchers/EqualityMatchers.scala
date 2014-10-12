package org.specs2
package guide
package matchers

import form.Card

object EqualityMatchers extends UserGuideCard {
  def title = "Equality"
  def text = s2"""
The most common type of matcher is `beEqualTo` to test the equality of 2 values. Several syntaxes can be used, according to your own taste

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a symbol
 `1 must_== 1              `| my favorite!
 `1 mustEqual 1            `| if you dislike underscores
 `1 should_== 1            `| for `should` lovers
 `1 === 1                  `| the ultimate shortcut
 `1 must be equalTo(1)     `| with a literate style

There are also other notions of equality

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `beTypedEqualTo           `| typed equality. `a must beTypedEqualTo(b)` will not work if `a` and `b` don't have compatible types
 `be_===                   `| synonym for `beTypedEqualTo`
 `a ==== b                 `| synonym for `a must beTypedEqualTo(b)`
 `a must_=== b             `| similar to `a must_== b` but will not typecheck if `a` and `b` don't have the same type
 `be_==~                   `| check if `(a: A) == conv(b: B)` when there is an implicit conversion `conv` from `B` to `A`
 `beTheSameAs              `| reference equality: check if `a eq b` (`a must be(b)` also works)
 `be                       `| `a must be(b)`: synonym for `beTheSameAs`
 `beTrue, beFalse          `| shortcuts for Boolean equality

_Note_: the `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`. So the `beEqualTo` matcher has been adapted to use the `.deep` method on `Arrays`, transforming them to `IndexedSeqs` (possibly nested), before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).

"""
}
case class Human(age: Int, wealth: Int)
