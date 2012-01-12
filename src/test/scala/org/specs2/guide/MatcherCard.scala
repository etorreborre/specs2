package org.specs2
package guide

import text.Markdown
import form.{TextCell, Tab, Form, Tabs}
import specification.Forms._
import Form._

trait MatcherCards extends Cards {
  def title = "Matchers"
  def cards = Seq(AnyMatchers, OptionEitherMatchers)
}
trait Cards {
  def title: String
  def cards: Seq[Card]
  def toTabs = Form(title).tabs(cards)((card: Card) => Tabs(Seq(card.toTab)))
}

trait Card {
  def title: String
  def text: String
  def toTab: Tab = Tab(title, Form.tr(TextCell(text).bkWhite))
}
object AnyMatchers extends Card {
  def title = "Matchers for Any"
  def text =  """
  The most common type of matcher is `beEqualTo` to test for equality. There are different ways to use this matcher:

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a shorter matcher
 `1 must_== 1              `| my favorite!
 `1 mustEqual 1            `| if you dislike underscores
 `1 should_== 1            `| for should lovers
 `1 === 1                  `| the ultimate shortcut
 `1 must be equalTo(1)     `| with a literate style

   *with a negation*        |
 -------------------------- |
 `1 must not be equalTo(2) `|
 `1 must_!= 2              `|
 `1 mustNotEqual 2         `|
 `1 must be_!=(2)          `|
 `1 !== 2                  `|

The `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`, for `Arrays`. So the `beEqualTo` matcher has been adapted to transform `Arrays` to `Seqs` before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).

You can see on the examples above several things which are applicable to all matchers:

 * the general form for using a matcher is `a must matcher`
 * you can use `should` instead of `must` if you prefer
 * there are only 2 shortcuts provided because the equality matcher is so ubiquitous `must_==` and `===`
 * for most of the matchers you can use a form where the ` be` word (or the `have` word) is detached
 * you can as well negate a matcher by adding not before it (or after it, as a method call)

An non-exhaustive list of those matchers:

 * `be_===` for checking if `a == b` where a and b are expected to have the same type by the compiler
 * `be_==~` for checking if `(a:A) == (b:A)` when there is an implicit conversion from B (the type of b) to A (the type of a)
 * `beTheSameAs` for checking if `a eq b` (`a must be(b)` also works)
 * `beTrue, beFalse`
 * `beLike { case exp => ok }`: to check if an object is like a given pattern (`ok` is a predefined value, `ko` is the opposite)
 * `beLike { case exp => exp must beXXX }`: to check if an object is like a given pattern, and verifies a condition
 * `beNull`
 * `beAsNullAs`: when 2 objects must be null at the same time if one of them is null
 * `beOneOf(a, b, c)`: to check if an object is one of a given list
 * `haveClass`: to check the class of an object
 * `haveSuperclass`: to check if the class of an object as another class as one of its ancestors
 * `haveInterface`: to check if an object is implementing a given interface
 * `beAssignableFrom`: to check if a class is assignable from another
 * `beAnInstanceOf[T]`: to check if an object is an instance of type `T`

  """
}

object OptionEitherMatchers extends Card {
  def title = "Option/Either Matchers"
  def text =  """
  There are several matchers to check Option and Either instances:

 * `beSome` checks if an element is Some(_)
 * `beSome.which(function)` checks if an element is Some(_) and satisfies a function returning a boolean
 * `beSome.like(partial function)` checks if an element is Some(_) and satisfies a partial function returning a `MatchResult`
 * `beNone` checks if an element is None
 * `beAsNoneAs` checks if 2 values are equal to None at the same time
 * `beRight` checks if an element is Right(_)
 * `beRight.like(partial function)` checks if an element is Right(_) and satisfies a partial function returning a `MatchResult`
 * `beLeft` checks if an element is Left(_)
 * `beLeft.like(partial function)` checks if an element is Left(_) and satisfies a partial function returning a `MatchResult`
  """
}
