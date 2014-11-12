package org.specs2
package guide

import java.io.File

import matcher._
import guide.matchers._
import org.specs2.specification.Forms

object Matchers extends UserGuidePage with Forms { def is = s2"""

The most frequent way to specify some expected behaviour with $specs2 is to use _matchers_. You generally execute an action, a command or a function and then check if the actual value you get is equal to an expected one (the ["arrange-act-assert"](http://bit.ly/arrange_act_assert) paradigm).

For example, if you create a specification for an object manipulating paths:${snippet{
// 8<---
object Paths { def directoryPath(p: String) = p+"/" }
// 8<---
// describe the functionality
s2"the directoryPath method should return well-formed paths $e1"

// give an example with some code
def e1 = Paths.directoryPath("/tmp/path/to/dir") must beEqualTo("/tmp/path/to/dir/")
}}

The `must` operator takes the actual value returned by `directoryPath` and applies it to a `Matcher` built with the expected value. `beEqualTo` is one of the many matchers defined by $specs2, it just checks if 2 values are equal.

In the following sections you will learn:

 - the different ways of checking the [equality](#equality) of values
 - to use the matchers for the most [common data types](#out-of-the-box) in Scala, and most notably `Traversable`
 - to use [other types of matchers](#optional) in less common situations: json, xml, files, parsers combinators...
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
Future(1) must be_>(0).await
Future { Thread.sleep(100); 1 } must be_>(0).await(retries = 2, timeout = 100.millis)
}}

 * use `await` to wait on a `Future[Matcher[T]]`: ${snippet{
  // 8<--
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._
  // 8<--
Future(1 === 1).await
Future(1 === 1).await(retries = 2, timeout = 100.millis)
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

 - use ${"standard results" ~/ StandardResults} (`failure`, `success`, `skipped`, `todo`...) instead of matchers
 - add ${"descriptions" ~/ ExpectationDescription} to your expectations to create even better failure messages
 - use ${"datatables" ~/ UseDatatables} to conveniently group several examples into one
 - use ${"ScalaCheck" ~/ UseScalaCheck} to generate and verify data for your examples
 - use ${"Mockito " ~/ UseMockito} to mock the interactions with another system
 - use ${"Forms" ~/ UseForms} to display actual and expected values in html tables

### And if you want to know more

 - read the ${"reference card" ~/ ReferenceCard} on all of $specs2 matchers
 - use ${"syntactic variations" ~/ SyntacticVariations} on the `value must matcher` form
 - implement the ${"`AsResult` typeclass" ~/ AsResultTypeclass} to go beyond matchers
 - use the ${"`beA[CaseClass]`" ~/ CaseClassMatchers} matcher to automatically create matchers for case classes
 - use the ${"dependencies matchers" ~/ DependencyMatchers} to specify dependencies between packages
 - use $specs2 matchers ${s"outside $specs2" ~/ OutsideSpecs2}

"""

<<<<<<< HEAD
=======
val settings = org.mockito.Mockito.withSettings
val m = mock[List[String]](settings)
}}

##### Stubbing

Stubbing values is as simple as calling a method on the mock and declaring what should be returned or thrown: ${snippet{

m.get(1) returns "one"
m.get(2) throws new RuntimeException("forbidden")
}}

You can specify different consecutive returned values by appending thenReturns or thenThrows: ${snippet{

m.get(1) returns "one" thenReturns "two"
m.get(2) throws new RuntimeException("forbidden") thenReturns "999"
}}

###### Mocking and Stubbing at the same time

It is also possible to create a mock while stubbing one of its methods, provided that you declare the type of the expected mock: ${snippet{

val mocked: java.util.List[String] = mock[java.util.List[String]].contains("o") returns true
mocked.contains("o") must beTrue
}}

##### With matchers

The built-in Mockito argument matchers can be used to specify the method arguments for stubbing: ${snippet{
m.get(anyInt()) returns "element"
m.get(999) must_== "element"
}}

***specs2*** matchers can also be passed directly as arguments: ${snippet{
m.get(===(123)) returns "one"
}}

**Note** the call above works because there is an implicit method `argThat` which transforms a specs2 `Matcher[T]` into a Hamcrest one and in turn call Mockito's `org.mockito.Matchers.argThat` method to register the Hamcrest matcher. However [sometimes](https://groups.google.com/forum/#!msg/specs2-users/_slOZQoICzU/DF-ZQCq_GmkJ) the implicit conversion is not called and you have to explicitly call the `argThat` method like so: ${snippet{
  m.get(argThat(===(123))) returns "one"
}}

##### Callbacks

In some rare cases, it is necessary to have the return value depend on the parameters passed to the mocked method: ${snippet{
m.get(anyInt) answers { i => "The parameter is " + i.toString }
}}

The function passed to `answers` will be called with each parameter passed to the stubbed method: ${snippet{
m.get(0)    // returns "The parameter is 0"
m.get(1)    // the second call returns a different value: "The parameter is 1"
}}

###### Parameters for the `answers` function

Because of the use of reflection the function passed to answers will receive only instances of the `java.lang.Object` type.

More precisely, it will:

* pass the mock object if both the method has no parameters and the function has one parameter:
`mock.size answers { mock => mock.hashCode }`
* pass the parameter if both the method and the function have one parameter:
`mock.get(0) answers { i => i.toString }`
* pass the parameter and the mock object if the method has 1 parameter and the function has 2:
`mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString }`

In any other cases, if `f` is a function of 1 parameter, the array of the method parameters will be passed and if the function has 2 parameters, the second one will be the mock.

##### Verification

By default Mockito doesn't expect any method to be called. However if your writing interaction-based specifications you want to specify that some methods are indeed called: ${snippet{
there was one(m).get(0)              // one call only to get(0)
there was no(m).get(0)               // no calls to get(0)

// were can also be used
there were two(m).get(0)             // 2 calls exactly to get(0)
there were three(m).get(0)           // 3 calls exactly to get(0)
there were 4.times(m).get(0)         // 4 calls exactly to get(0)

there was atLeastOne(m).get(0)       // at least one call to get(0)
there was atLeastTwo(m).get(0)       // at least two calls to get(0)
there was atLeastThree(m).get(0)     // at least three calls to get(0)
there was atLeast(4)(m).get(0)       // at least four calls to get(0)

there was atMostOne(m).get(0)        // at most one call to get(0)
there was atMostTwo(m).get(0)        // at most two calls to get(0)
there was atMostThree(m).get(0)      // at most three calls to get(0)
there was atMost(4)(m).get(0)        // at most four calls to get(0)

// the combinators above, except `atMost`, can also be used with a timeout
there was after(10.millis).one(m).get(0)
there was after(2.seconds).two(m).get(0)

}}

It is also possible to add all verifications inside a block, when several mocks are involved: ${snippet{

got {
  one(m).get(0)
  two(m).get(1)
}
}}

###### Order of calls

The order of method calls can be checked by creating calls and chaining them with `andThen`: ${snippet{
val m1 = mock[List[String]]

m1.get(0)
m1.get(1)

there was one(m1).get(0) andThen one(m1).get(1)
}}
when several mocks are involved, the expected order must be specified as an implicit value: ${snippet{
val m1 = mock[List[String]]
val m2 = mock[List[String]]
val m3 = mock[List[String]]

// the order of mock objects doesn't matter here
implicit val order = inOrder(m1, m3, m2)

m1.get(1); m2.get(2); m3.get(3)

there was one(m1).get(1) andThen one(m2).get(2) andThen one(m3).get(3)
}}

###### Ignoring stubs

When specifying the behavior of an object in relation to others you may want to verify that some mocks have been called as collaborators and you don't really want to specify what happens to other mocks because they are just playing the role of stubs.

In this case the `ignoreStubs` method can be used: ${snippet{

val (stub1, stub2) = (mock[AStub], mock[AStub])
there were noMoreCallsTo(ignoreStubs(stub1, stub2))
}}

This method is also available with the `inOrder` method: ${snippet{
val (list1, list2) = ("", "")
// 8<--
implicit val order = inOrder(ignoreStubs(list1, list2))
}}

For more documentation about this Mockito functionality, please read [here](http://docs.mockito.googlecode.com/hg/1.9.0/org/mockito/Mockito.html#25).

###### Spies

Spies can be used in order to do some "partial mocking" of real objects: ${snippet{

val spiedList = spy(new LinkedList[String])

// methods can be stubbed on a spy
spiedList.size returns 100

// other methods can also be used
spiedList.add("one")
spiedList.add("two")

// and verification can happen on a spy
there was one(spiedList).add("one")
}}

However, working with spies can be tricky: ${snippet{
// 8<--
val spiedList = spy(new LinkedList[String])
// 8<--
// if the list is empty, this will throws an IndexOutOfBoundsException
spiedList.get(0) returns "one"
}}

As advised in the Mockito documentation, doReturn must be used in that case: ${snippet{
// 8<--
val spiedList = spy(new LinkedList[String])
// 8<--
org.mockito.Mockito.doReturn("one").when(spiedList).get(0)
}}

###### Functions/PartialFunctions

It is possible to verify method calls where parameters are functions by specifying how the passed function will react to a given set of arguments. Given the following mock:

```
trait Amount {
// a method showing an amount precision
def show(display: Function2[Double, Int, String]) = ???
}
val amount = mock[Amount]
```

If the mock is called with this function: ${snippet{
// 8<--
val amount = mock[Amount]
// 8<--
amount.show((amount: Double, precision: Int) => "%2."+precision+"f" format amount)
}}

Then it is possible to verify how the mock was called: ${snippet{
// 8<--
val amount = mock[Amount]
// 8<--
// with sample arguments for the function and the expected result
there was one(amount).show((32.4456, 2) -> "32.45")

// with a matcher for the result
there was one(amount).show((32.4456, 2) -> endWith("45"))

// with any Function2[A, B, R]
there was one(amount).show(anyFunction2)
}}

###### Auto-boxing

Auto-boxing might interfere with the mocking of PartialFunctions. Please have a look at [this](https://groups.google.com/d/topic/specs2-users/_bK8lCCjZ4c/discussion) for a discussion.

###### Byname

Byname parameters can be verified but this will not work if the specs2 jar is not put first on the classpath, before the mockito jar. Indeed specs2 redefines a Mockito class for intercepting method calls so that byname parameters are properly handled.

### DataTables

DataTables are a very effective way of grouping several similar examples into one. For example, here is how to specify the addition of integers by providing one example on each row of a table: ${snippet{

class DataTableSpec extends Specification with matcher.DataTables { def is =
  "adding integers should just work in scala"  ! e1

  def e1 =
    "a"   | "b" | "c" |                                   // the header of the table, with `|` separated strings
     2    !  2  !  4  |                                   // an example row
     1    !  1  !  2  |> {                                // the > operator to "execute" the table
     (a, b, c) =>  a + b must_== c                        // the expectation to check on each row
    }
}
}}

#### Implicit !

There may be an implicit definition conflict when the first parameter of a row is a String, because examples can also be created by using the `!` operator after a String. In that case, depending on which kind of specification you use, you can either:

* with an acceptance specification: use the `!!` operator to disambiguate (and `||` in the header for good visual balance)
* with a unit specification: use the `org.specs2.mutable.Tables` trait instead of `org.specs2.matcher.DataTables` trait. This will "deactivate" the implicit used to create examples with `!`

### Forms

Forms are a way to represent domain objects or service, and declare expected values in a tabular format. They are supposed to be used with the HtmlRunner to get human-readable documentation.

Forms can be designed as reusable pieces of specification where complex forms can be built out of simple ones.

""" ^
  "Here's " ~ ("how to use Forms", new org.specs2.guide.FormsPage) ^
s2"""

### Outside specs2

The ***specs2*** matchers are a well-delimited piece of functionality that you should be able to reuse in your own test framework. You can reuse the following traits:

 * `${fullName[matcher.MustMatchers]}` (or `${fullName[matcher.ShouldMatchers]}`) to write anything like `1 must be_==(1)` and
   get a `Result` back

 * **Important**: the `MustMatchers` *trait* will fill-in stacktraces on `MatchResults` while the `MustMatchers` object will not. This has some important consequences in terms of performances because creating stack traces is expensive

 * You can also use the side-effecting version of that trait called `${fullName[matcher.MustThrownMatchers]}` (or `${fullName[matcher.ShouldThrownMatchers]}`).
   It throws a `FailureException` as soon as an expectation is failing. Those traits can also be used in a regular
   Specification if you have several expectations per example and if you don't want to chain them with `and`.

 * Finally, in a JUnit-like library you can use the `org.specs2.matcher.JUnitMustMatchers` trait which throws
   `AssertionFailureError`s

#### Without any dependency on specs2

The [Testing](https://github.com/spray/spray/wiki/Testing) page of the ***spray*** project explains how you can define a testing trait in your library which can be used with specs2 or scalatest or any framework defining the following methods:

   * `fail(msg: String): Nothing`
   * `skip(msg: String): Nothing`

In specs2, those 2 methods are defined by the `${fullName[matcher.ThrownMessages]}` trait

    trait ThrownMessages { this: ThrownExpectations =>
      def fail(m: String): Nothing = failure(m)
      def skip(m: String): Nothing = skipped(m)
    }

   - - -

                                                                                                                         """ ^
                                                                                                                        br ^
  include(xonly, examples)                                                                                              ^
  include(xonly, akaExpectations)                                                                                       ^
  include(xonly, scalaCheckExamples)                                                                                    ^
  include(xonly, new MockitoSpecification)                                                                              ^
  include(xonly, new DataTableSpecification)                                                                            ^
  include(xonly, mockitoExamples)                                                                                       ^
  include(xonly, jsonExamples)                                                                                          ^
  include(xonly, new ParserSpec)                                                                                        ^
  end

  def m: java.util.List[String] = mock[java.util.List[String]]
  trait AStub

  lazy val examples = new Specification { def is = "Examples".title ^
    "This is hopefully true"         ! (1 != 2)     ^
    { 1 must beEqualTo(1)      }                    ^
    { 1 must_== 1              }                    ^ // my favorite!
    { 1 should_== 1            }                    ^ // for should lovers
    { 1 === 1                  }                    ^ // the ultimate shortcut
    { 1 must be equalTo(1)     }                    ^ // with a literate style
    { 1 must not be equalTo(2) }                    ^ // with a negation
                                                    end
    def beShort = be_<=(5) ^^ { (t: Any) => t.toString.size }
  }

 lazy val akaExpectations = new Specification { def is = "Aka".title ^
    "without description"                                        ! {
      machine.tickets must have size(3)
    }^
    "with description"                                           ! {
      machine.tickets aka "the created tickets" must have size(3)
    }
    case class Machine(tickets: List[String])
    val machine = Machine(List("ticket1", "ticket2", "ticket3"))
  }

 lazy val scalaCheckExamples = new Specification with ScalaCheck {
    import org.scalacheck._
    implicit val params = set(minTestsOk = 20)

    def is = "Scalacheck".title ^
    "addition and multiplication are related" ! Prop.forAll { (a: Int) => a + a == 2 * a }             ^
    "addition and multiplication are related" ! prop { (a: Int) => a + a == 2 * a }                    ^
    "addition and multiplication are related" ! prop { (a: Int) => a + a must_== 2 * a }               ^
    "addition and multiplication are related" ! prop { (a: Int) => (a > 0) ==> (a + a must_== 2 * a) } ^
    "this is a specific property" ! prop { (a: Int, b: Int) =>
      (a + b) must_== (b + a)
    }.set(minTestsOk = 200, workers = 1)                                                             ^
                                                                                                       end
  }

  import org.specs2.matcher._

  class MyOwn extends Matcher[String] {
    def apply[S <: String](s: Expectable[S]) = {
      result(s.value.isEmpty,
             s.description + " is empty",
             s.description + " is not empty",
             s)
    }
  }
  
  import org.specs2.mock._
  class MockitoSpecification extends Specification { def is =

     "A java list can be mocked"                                                    ^
       "You can make it return a stubbed value"                                     ! c().stub^
       "You can verify that a method was called"                                    ! c().verify^
       "You can verify that a method was not called"                                ! c().verify2^
                                                                                    end
     case class c() extends Mockito {
       val m = mock[java.util.List[String]] // a concrete class would be mocked with: mock[new java.util.LinkedList[String]]
       def stub = {
         m.get(0) returns "one"             // stub a method call with a return value
         m.get(0) must_== "one"             // call the method
       }
       def verify = {
         m.get(0) returns "one"             // stub a method call with a return value
         m.get(0)                           // call the method
         there was one(m).get(0)            // verify that the call happened
       }
       def verify2 = there was no(m).get(0)      // verify that the call never happened
     }
   }
  lazy val mockitoExamples = new Specification { def is =
     "returned values"                         ! c().e1 ^
     "consecutive returns"                     ! c().e2 ^
     "matchers"                                ! c().e3 ^
                                               end
     import org.mockito.Matchers._
     
     case class c() extends Mockito {
       val m = mock[java.util.List[String]]
       def e1 = {
         m.get(1) returns "one"
         m.get(2) throws new RuntimeException("forbidden")
         success
       }
       def e2 = {
         m.get(1) returns "one" thenReturns "two"
         m.get(2) throws new RuntimeException("forbidden") thenReturns "999"
         success
       }
       def e3 = {
         m.get(anyInt) returns "element"
         m.get(999) must_== "element"
         m.get(===(123)) returns "one"
         success
       }
     }
   }

  import matcher._
  class DataTableSpecification extends Specification with DataTables { def is =
    "adding integers should just work in scala"  ! e1

    def e1 =
      "a"   | "b" | "c" |
       2    !  2  !  4  |
       1    !  1  !  2  |> { (a, b, c) =>  a + b must_== c }
  }

  lazy val jsonExamples = new JsonExamples
}


class JsonExamples extends Specification with matcher.JsonMatchers {
    val person = """{
      "person": {
        "name": "Joe",
        "age": 35,
        "spouse": {
          "person": {
            "name": "Marilyn",
            "age": 33
          }
        }
      }
    }"""

    def is =
    "1" ! { person must /("person") */("person") /("age" -> 33.0) }
    "2" ! { person must /("person") /#(2) /("person") }
}

case class Human(age: Int, wealth: Int)
trait Amount { def show(display: Function2[Double, Int, String]) = "" }

import util.parsing.combinator.RegexParsers
import NumberParsers.{number, error}

class ParserSpec extends Specification with matcher.ParserMatchers {  def is =
  "Parsers for numbers"                                                                   ^
                                                                                          p^
  "beASuccess and succeedOn check if the parse succeeds"                                  ^
  { number("1") must beASuccess }                                                         ^
  { number must succeedOn("12") }                                                         ^
  { number must succeedOn("12").withResult(12) }                                          ^
  { number must succeedOn("12").withResult(equalTo(12)) }                                 ^
  { number("1") must haveSuccessResult("1") }                                             ^
                                                                                          p^
  "beAFailure and failOn check if the parse fails"                                        ^
  { number must failOn("abc") }                                                           ^
  { number must failOn("abc").withMsg("string matching regex.*expected") }                ^
  { number must failOn("abc").withMsg(matching(".*string matching regex.*expected.*")) }  ^
  { number("i") must beAFailure }                                                         ^
  { number("i") must haveFailureMsg("i' found") }                                         ^
                                                                                          p^
  "beAnError and errorOn check if the parser errors out completely"                       ^
  { error must errorOn("") }                                                              ^
  { error("") must beAnError }                                                            ^
                                                                                          end

  val parsers = NumberParsers
}
object NumberParsers extends RegexParsers {
  /** parse a number with any number of digits */
  val number: Parser[Int] = "\\d+".r ^^ {_.toInt}
  /** this parser returns an error */
  val error: Parser[String] = err("Error")
>>>>>>> 9fcd3c8... made the MustMatchers object *not* fill-in stacktraces and documented that
}
