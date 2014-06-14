package org.specs2
package guide
package matchers

import form.Card

object EqualityMatchers extends Card {
  def title = "Equality"
  def text = s2"""
The most common type of matcher is `beEqualTo` to test for equality. It is so frequent that there are several equivalent ways to declare equality

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a symbol
 `1 must_== 1              `| my favorite!
 `1 mustEqual 1            `| if you dislike underscores
 `1 should_== 1            `| for should lovers
 `1 === 1                  `| the ultimate shortcut, synonym for `1 must beEqualTo(1)`
 `1 must be equalTo(1)     `| with a literate style

There are also other notions of equality

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `beTypedEqualTo           `| typed equality. `a must beTypedEqualTo(b)` will not work if `a` and `b` don't have compatible types
 `be_===                   `| synonym for `beTypedEqualTo`
 `a ==== b                 `| synonym for `a must beTypedEqualTo(b)`
 `be_==~                   `| check if `(a: A) == conv(b: B)` when there is an implicit conversion `conv` from `B` to `A`
 `beTheSameAs              `| reference equality: check if `a eq b` (`a must be(b)` also works)
 `be                       `| `a must be(b)`: synonym for `beTheSameAs`
 `beTrue, beFalse          `| shortcuts for Boolean equality

_Note_: the `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`, for `Arrays`. So the `beEqualTo` matcher has been adapted to transform `Arrays` to `Seqs` before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).

"""
}
case class Human(age: Int, wealth: Int)
