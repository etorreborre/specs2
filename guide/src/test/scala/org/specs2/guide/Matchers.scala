package org.specs2
package guide

import java.io.File

import matcher._
import guide.matchers._
import org.specs2.specification.Forms

object Matchers extends UserGuidePage with Forms { def is = s2"""

The most frequent way to specify some expected behaviour with specs2 is to use _matchers_. You generally execute an action, a command or a function and then check if the actual value you get is equal to an expected one (the ["arrange-act-assert"](http://bit.ly/arrange_act_assert) paradigm). For example, if you create a specification for an object manipulating paths:${snippet{
// 8<---
object Paths { def directoryPath(p: String) = p+"/" }
// 8<---
// describe the functionality
s2"the directoryPath method should return well-formed paths $e1"

// give an example with some code
def e1 = Paths.directoryPath("/tmp/path/to/dir") must beEqualTo("/tmp/path/to/dir/")
}}

The `must` operator takes the actual value returned by `directoryPath` and applies it to a `Matcher` built with the expected value. `beEqualTo` is one of the many matchers defined by ***specs2***, it just checks if 2 values are equal. In the following sections you will learn:

 - the different ways of checking [equality](#equality)
 - to use the matchers for the most [common data types](#out-of-the-box) in Scala, and most notably `Traversable`
 - to use [other types of matchers](Optional) in less common situations, for: json, xml, files, parsers combinators...
 - how to [derive](#derive-matchers) a new matcher from an existing one
 - how to create [your own matchers](#create-your-own)

### Equality

${EqualityMatchers.text}

Now let's check the other matchers.

### Out of the box

These are the all the available matchers when you extend `Specification`

${ MatcherCards.toTabs }

### Optional

Those matchers are optional. To use them, you need to add a new trait to your specification:

${ OptionalMatcherCards.toTabs }

### Derive matchers

The easiest way to create a new matcher is to derive it from an existing one. You can:

 * use logical operators ${snippet{

  def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)

}}

 * "adapt" the actual value ${snippet {

  // This matcher adapts the existing `be_<=` matcher to a matcher applicable to `Any`
  def beShort1 = be_<=(5) ^^ { (t: Any) => t.toString.size }

  // you can use aka to provide some information about the original value, before adaptation
  def beShort2 = be_<=(5) ^^ { (t: Any) => t.toString.size aka "the string size" }

  // !!! note: use a BeTypedEqualTo matcher when using aka and equality, otherwise you will be matching against Expectable[T] !!!
  def beFive = be_===(5) ^^ { (t: Any) => t.toString.size aka "the string size" }

  // The adaptation can also be done the other way around when it's more readable
  def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
}}

 * adapt the actual and expected values. This matcher compares 2 `Human` objects but set their `wealth` field to 0
   so that the equals method will not fail on that field: ${snippet{

  def beMostlyEqualTo = (be_==(_:Human)) ^^^ ((_:Human).copy(wealth = 0))
  // then
  Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1)) // success
}}

 * use `eventually` to try a match a number of times until it succeeds: ${snippet{

  val iterator = List(1, 2, 3).iterator

  // Use eventually(retries, n.millis) to use another number of tries and waiting time
  iterator.next must be_==(3).eventually
}}

 * use `await` to create a matcher that will match on `Matcher[Future[T]]`: ${snippet{
  // 8<--
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._
  // 8<--
  future(1) must be_>(0).await
  future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis)
}}

 * use `await` to wait on a `Future[Matcher[T]]`: ${snippet{
  // 8<--
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._
  // 8<--
  future(1 === 1).await
  future(1 === 1).await(retries = 2, timeout = 100.millis)
}}

 * use `when` or `unless` to apply a matcher only if a condition is satisfied: ${snippet{

  1 must be_==(2).when(false)                        // will return a success
  1 must be_==(2).unless(true)                       // same thing

  1 must be_==(2).when(false, "don't check this")    // will return a success
  1 must be_==(2).unless(true, "don't check this")   // same thing
}}

 * use `iff` to say that a matcher must succeed if and only if a condition is satisfied: ${snippet{

  1 must be_==(1).iff(true)                        // will return a success
  1 must be_==(2).iff(true)                        // will return a failure
  1 must be_==(2).iff(false)                       // will return a success
  1 must be_==(1).iff(false)                       // will return a failure
}}

 * use `orSkip` to return a `Skipped` result instead of a Failure if the condition is not satisfied ${snippet{

  1 must be_==(2).orSkip
  1 must be_==(2).orSkip("Precondition failed")    // prints "Precondition failed: '1' is not equal to '2'"
  1 must be_==(2).orSkip((ko:String) => "BAD "+ko) // prints "BAD '1' is not equal to '2'"
}}

 * use `orPending` to return a `Pending` result instead of a Failure if the condition is not satisfied ${snippet{

  1 must be_==(2).orPending
  1 must be_==(2).orPending("Precondition failed")    // prints "Precondition failed: '1' is not equal to '2'"
  1 must be_==(2).orPending((ko:String) => "BAD "+ko) // prints "BAD '1' is not equal to '2'"
}}

 * use `zip` operators for to match each value of a tuple ${snippet{

  type MyTuple = (String, String, String, Seq[(String, Double)])

  val t1: MyTuple = ("a", "b", "c", Seq(("d", 1.01), ("e", 2.02)))
  val t2: MyTuple = ("a", "b", "c", Seq(("d", 1.00), ("e", 2.00)))

  // create a matcher by zipping matchers to the expected value
  def beMatching(expected: MyTuple) = expected.zip(startWith, ===, ===, matchSequence)
  // match the elements of a sequence with a zipped matcher using string equality for the first field and
  // approximate Double equality for the second field
  def matchSequence(expected: =>Seq[(String, Double)]) = expected.contain(_.zip(===, ==~)).inOrder

  /** type inference doesn't work if this matcher, specialised to Double, is not defined */
  def ==~(d: =>Double) = beCloseTo(d +/- 0.1)

  t1 must beMatching(t2)
}}

### Create your own

The easiest way to create a new matcher is to create it from a function returning a tuple with a boolean and one or more messages: ${snippet{

  // annotate the return type so that implicit conversions can transform your function into a Matcher object
  // here just return a boolean and a failure message
  def m1: Matcher[String] = { s: String =>
    (s.startsWith("hello"), s+" doesn't start with hello")
  }

  // with a success message and a failure message
  def m2: Matcher[String] = { s: String =>
    (s.startsWith("hello"), s+" starts with hello", s+" doesn't start with hello")
  }

  // with a function taking the actual value for the failure message
  def m3: Matcher[String] =
    ((_: String).startsWith("hello"), (_:String)+" doesn't start with hello")

  // with 2 functions for the success and failure messages
  def m4: Matcher[String] =
    ((_: String).startsWith("hello"), (s:String) => s+ " starts with hello", (s:String) => s+ " doesn't start with hello")

}}

If you want absolute power over matching, you can define your own matcher extending `Matcher`: ${snippet{

  class MyOwn extends Matcher[String] {
    def apply[S <: String](s: Expectable[S]) = {
      result(s.value.isEmpty,
        s.description + " is empty",
        s.description + " is not empty",
        s)
    }
  }
}}

In the code above you have to:

 * define the `apply` method (and its somewhat complex signature)

 * use the protected `result` method to return: a Boolean condition, a message when the match is ok, a message when the
   match is not ok, the "expectable" value. Note that if you change the expectable value you need to use the `map` method
   on the `s` expectable (`s.map(other)`). This way you preserve the ability of the Expectable to throw an Exception if
   a subsequent match fails

 * you can use the `description` method on the `Expectable` class to return the full description of the expectable including
   the optional description you setup using the `aka` method


### Now learn how to...

 - use ${"standard results" ~ StandardResults} (`failure`, `success`, `skipped`, `todo`...) instead of matchers
 - add descriptions to your expectations to create even better failure messages
 - use datatables to conveniently group several examples into one
 - use ScalaCheck to generate and verify data for your examples
 - use Mockito to mock the interactions with another system
 - use `Forms` to display actual and expected values in html tables

### And if you want to know more

 - read the reference card on all of ***specs2*** matchers
 - use syntactic variations on the `value must matcherOf(expected)` form
 - implement the `AsResult` typeclass to go beyond matchers
 - use the `beA[CaseClass]` matcher to automatically create matchers for case classes
 - use the `Analysis` matchers to specify dependencies between packages

"""

}
