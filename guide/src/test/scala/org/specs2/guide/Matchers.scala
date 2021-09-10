package org.specs2
package guide

import java.io.File
import matcher.*
import guide.matchers.*
import org.specs2.concurrent.ExecutionEnv
import org.specs2.specification.Forms

object Matchers extends UserGuidePage with Forms {
  def is = s2"""

The most frequent way to specify some expected behaviour with $specs2 is to use _matchers_. You generally execute an action, a command or a function and then check if the actual value you get is equal to an expected one (the ["arrange-act-assert"](http://bit.ly/arrange_act_assert) paradigm).

For example, if you create a specification for an object manipulating paths:${snippet{
// 8<---
object Paths { def directoryPath(p: String) = p + "/" }
// 8<---
// describe the functionality
s2"the directoryPath method should return well-formed paths $e1"

// give an example with some code
def e1 = Paths.directoryPath("/tmp/path/to/dir") must beEqualTo("/tmp/path/to/dir/")
}}

The `must` operator takes the actual value returned by `directoryPath` and applies it to a `Matcher` built with the expected value. `beEqualTo` is one of the many matchers defined by $specs2, it just checks if 2 values are equal.

In the following sections you will learn:

 - the different ways of checking the [equality](#equality) of values
 - how to use the matchers for the most [common data types](#out-of-the-box) in Scala, and most notably collections
 - how to use [other types of matchers](#optional) in less common situations: json, xml, files,...
 - how to [derive](#derive-matchers) a new matcher from an existing one
 - how to create [your own matchers](#create-your-own)

### Equality

${EqualityMatchers.text}

Now let's check out the other matchers.

### Out of the box

These are the all the available matchers when you extend `Specification`:

${MatcherCards.toTabs}

### Optional

Those matchers are optional. To use them, you need to add a new trait to your specification.

Those are additional "data" matchers:

${OptionalDataMatcherCards.toTabs}

Those matchers can be used to check "content":

${OptionalContentMatcherCards.toTabs}

And finally those matchers are Scala / Language related

${OptionalLanguageMatcherCards.toTabs}


### Derive matchers

The easiest way to create a new matcher is to derive it from an existing one. You can:

 * use logical operators ${snippet{
    def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)
  }}

 * "adapt" the actual value ${snippet{

// This matcher adapts the existing `be_<=` matcher to a matcher applicable to `Any`
def beShort1 = be_<=(5) ^^ { (t: Any) => t.toString.length }

// you can use aka to provide some information about the original value, before adaptation
def beShort2 = be_<=(5) ^^ { (t: Any) => t.toString.length aka "the string size" }

// The adaptation can also be done the other way around when it's more readable
def haveExtension(extension: =>String) = ((_: File).getPath) ^^ endWith(extension)
}}

 * adapt the actual and expected values. This matcher compares 2 `Human` objects but set their `wealth` field to 0
   so that the equals method will not fail on that field: ${snippet{

def beMostlyEqualTo(h: Human) = be_==(h) ^^^ ((_: Human).copy(wealth = 0))
// then
Human(age = 20, wealth = 1000) must beMostlyEqualTo(Human(age = 20, wealth = 1)) // success
}}

 * use `eventually` to try a matcher a number of times until it succeeds: ${snippet{

val iterator = List(1, 2, 3).iterator

// Use eventually(retries, n.millis) to specify the number of tries and waiting time
iterator.next must be_==(3).eventually
}}

 * use `await` to create a matcher that will match on `Matcher[Future[T]]` (this requires an ${"execution environment" ~/ ExecutionEnvironments}): ${snippet{
// 8<--
import scala.concurrent.*
import scala.concurrent.duration.*
given ee: ExecutionEnv = ???
// 8<--
Future(1) must be_>(0).await
Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis)
}}

 * use `when` or `unless` to apply a matcher only if a condition is satisfied: ${snippet{
1 must be_==(2).when(false) // will return a success
1 must be_==(2).unless(true) // same thing

1 must be_==(2).when(false, "don't check this") // will return a success
1 must be_==(2).unless(true, "don't check this") // same thing
}}

 * use `iff` to say that a matcher must succeed if and only if a condition is satisfied: ${snippet{
1 must be_==(1).iff(true) // will return a success
1 must be_==(2).iff(true) // will return a failure
1 must be_==(2).iff(false) // will return a success
1 must be_==(1).iff(false) // will return a failure
}}

 * use `orSkip` to return a `Skipped` result instead of a Failure if the condition is not satisfied ${snippet{
1 must be_==(2).orSkip
1 must be_==(2).orSkip("Precondition failed") // prints "Precondition failed: '1' is not equal to '2'"
1 must be_==(2).orSkip((ko: String) => "BAD " + ko) // prints "BAD '1' is not equal to '2'"
}}

 * use `orPending` to return a `Pending` result instead of a Failure if the condition is not satisfied ${snippet{
1 must be_==(2).orPending
1 must be_==(2).orPending("Precondition failed") // prints "Precondition failed: '1' is not equal to '2'"
1 must be_==(2).orPending((ko: String) => "BAD " + ko) // prints "BAD '1' is not equal to '2'"
}}

### Create your own

The easiest way to create a new matcher is to create it from a function returning a tuple with a boolean and one or more messages: ${snippet{

// import the necessary implicit conversions if you are outside of a Specification
// import org.specs2.matcher.Matcher.{given}

// annotate the return type so that implicit conversions can transform your function into a Matcher object
// here just return a boolean and a failure message
def startWithHello: Matcher[String] = { (s: String) =>
  (s.startsWith("hello"), s + " doesn't start with hello")
}
}}

If you want absolute power over matching, you can define your own matcher extending `Matcher`: ${snippet{

import org.specs2.execute.Result.*

case class BeMyOwnEmpty() extends Matcher[String] {
  def apply[S <: String](s: Expectable[S]) = {
    result(s.value.isEmpty, s.description + " is not empty")
  }
}

"" must BeMyOwnEmpty()
}}

In the code above you have to:

 * define the `apply` method (and its somewhat complex signature)

 * use the protected `result` method to return a `Boolean` condition and a failure message

 * you can use the `description` method on the `Expectable` class to return the full description of the expectable including
   the optional description you setup using the `aka` method

$NowLearnTo

 - use ${"standard results" ~/ StandardResults} (`failure`, `success`, `skipped`, `todo`...) instead of matchers
 - add ${"descriptions" ~/ ExpectationDescription} to your expectations to create even better failure messages
 - use ${"datatables" ~/ UseDatatables} to conveniently group several examples into one
 - use ${"ScalaCheck" ~/ UseScalaCheck} to generate and verify data for your examples
 - use ${"Forms" ~/ UseForms} to display actual and expected values in html tables

$vid

$AndIfYouWantToKnowMore

 - read the ${"reference card" ~/ ReferenceCard} on all of $specs2 matchers
 - implement the ${"`AsResult` typeclass" ~/ AsResultTypeclass} to go beyond matchers
 - use $specs2 matchers ${s"outside <s2>specs2</s2>" ~/ OutsideSpecs2}

$vid
"""

}
