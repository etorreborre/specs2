package org.specs2
package guide

import specification.Forms._

class Matchers extends Specification { def is = literate ^ "Matchers guide".title ^
"""
There are many ways to define expectations in ***specs2***. You can define expectations with anything that returns a `Result`:

  * Boolean
  * Standard result
  * Matcher result
  * Scalacheck property
  * Mock expectation
  * DataTable
  * Forms

### Boolean results

This is the simplest kind of result you can define for an expectation but also the least expressive!

Here's an example:

    "This is hopefully true"         ! (1 != 2)

This can be useful for simple expectations but a failure will give few information on what went wrong:

    "This is hopefully true"         ! (2 != 2) // fails with 'the value is false',...

### Standard results

Some standard results can be used when you need specific result meanings:

  * `success`: the example is ok
  * `failure`: there is a non-met expectation
  * `anError`: a non-expected exception occurred
  * `skipped`: the example is skipped possibly at runtime because some conditions are not met. A more specific message can
    be created with `Skipped("my message")`
  * `pending`: usually means "not implemented yet", but a specific message can be created with `Pending("my message")`

Two additional results are also available to track the progress of features:

  * `done`: a `Success` with the message "DONE"
  * `todo`: a `Pending` with the message "TODO"

### Match results

This is by far the largest category of Results in ***specs2***. They cover many data types, can be composed and adapted to create new ones or be created from scratch by the user.

#### Out-of-the-box matchers

The most common matchers are automatically available when extending the `Specification` trait:
"""^
  MatcherCards.toTabs^p^
"""
The examples above show how to use matchers:

 * the general form for using a matcher is: `a must matcher`
 * but can use `should` instead of `must` if you prefer
 * for most matchers you can use a form where the ` be` word (or the `have` word) is detached
 * you can as well negate a matcher by adding `not` before it (or after it, as a method call)

#### Optional matchers

These other matchers need to be selectively added to the specification by adding a new trait:
"""^
  OptionalMatcherCards.toTabs^p^
"""

#### With a better description

Most of the time, the message displayed in the case of a matcher failure is clear enough. However a bit more information is sometimes necessary to get a better diagnostic on the value that's being checked. Let's say that you want to check a "ticket list":

      // will fail with "List(ticket1, ticket2) doesn't have size 3" for example
      machine.tickets must have size(3) // machine is a user-defined object

If you wish to get a more precise failure message you can set an alias with the `aka` method (*also known as*):

      // will fail with "the created tickets 'List(ticket1, ticket2)' doesn't have size 3"
      machine.tickets aka "the created tickets" must haveSize(3)

There is also a shortcut for `value aka value.toString` which is simply `value.aka`.

And when you want to other ways to customize the description, you can use:

 * `post`: `"a" post "is the first letter"` prints `a is the first letter`
 * `as`: `"b" as ((s:String) => "a"+s+"c")` prints `abc`


#### Matchers creation

There are many ways to create matchers for your specific usage. The simplest way is to reuse the existing ones:

 * using logical operators

        def beBetween(i: Int, j: Int) = be_>=(i) and be_<=(j)

        // create a Seq Matcher from a Matcher
        def allBeGreaterThan2: Matcher[Seq[Int]]   = be_>=(2).forall     // fail after the first failure
        def allBeGreaterThan3: Matcher[Seq[Int]]   = be_>=(2).foreach    // like forall but execute all matchers and collect the results
        def haveOneGreaterThan2: Matcher[Seq[Int]] = be_>=(2).atLeastOnce

 * adapting the actual value. This matcher adapts the existing `be_<=` matcher to a matcher applicable to `Any`

        def beShort = be_<=(5) ^^ { (t: Any) => t.toString.size }
        def beShort = be_<=(5) ^^ { (t: Any) => t.toString.size aka "the string size" }

 * adapting the actual and expected values. This matcher compares 2 `Human` objects but set their `wealth` field to 0
   so that the equals method will not fail on that field:

        def beMostlyEqualTo = (be_==(_:Human)) ^^^ ((_:Human).copy(wealth = 0))
        // then
        Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1)) toResult // success

 * using `eventually` to try a match a number of times until it succeeds:

        val iterator = List(1, 2, 3).iterator
        iterator.next must be_==(3).eventually
         // Use eventually(retries, n.millis) to use another number of tries and waiting time

 * using `when` or `unless` to apply a matcher only if a condition is satisfied:

        1 must be_==(2).when(false)                        // will return a success
        1 must be_==(2).unless(true)                       // same thing

        1 must be_==(2).when(false, "don't check this")    // will return a success
        1 must be_==(2).unless(true, "don't check this")   // same thing

 * using `iff` to say that a matcher must succeed if and only if a condition is satisfied:

        1 must be_==(1).iff(true)                        // will return a success
        1 must be_==(2).iff(true)                        // will return a failure
        1 must be_==(2).iff(false)                       // will return a success
        1 must be_==(1).iff(false)                       // will return a failure

 * using `orSkip` to return a `Skipped` result instead of a Failure if the condition is not met

        1 must be_==(2).orSkip
        1 must be_==(2).orSkip("Precondition failed")  // prints "Precondition failed: '1' is not equal to '2'"

 * using `mute` to change a Matcher so that it returns MatchResults with no messages. This is used in Forms to create
   properties showing no messages when they fail

Another easy way to create matchers, is to use some implicit conversions from functions to Matchers:

       val m: Matcher[String]  = ((_: String).startsWith("hello"), "doesn't start with hello")
       val m1: Matcher[String] = ((_: String).startsWith("hello"), "starts with hello", "doesn't start with hello")
       val m2: Matcher[String] = ((_: String).startsWith("hello"), (s:String) => s+ " doesn't start with hello")
       val m3: Matcher[String] = ((_: String).startsWith("hello"), (s:String) => s+ " starts with hello", (s:String) => s+ " doesn't start with hello")
       val m4: Matcher[String] = (s: String) => (s.startsWith("hello"), s+" doesn't start with hello")
       val m5: Matcher[String] = (s: String) => (s.startsWith("hello"), s+ "starts with hello", s+ " doesn't start with hello")

And if you want absolute power over matching, you can define your own matcher:

      class MyOwn extends Matcher[String] {
        def apply[S <: String](s: Expectable[S]) = {
          result(s.value.isEmpty,
                 s.description + " is empty",
                 s.description + " is not empty",
                 s)
        }
      }

In the code above you have to:

 * define the `apply` method (and its somewhat complex signature)

 * use the protected `result` method to return: a Boolean condition, a message when the match is ok, a message when the
   match is not ok, the "expectable" value. Note that if you change the expectable value you need to use the `map` method
   on the `s` expectable (`s.map(other)`). This way you preserve the ability of the Expectable to throw an Exception if
   a subsequent match fails

 * you can use the `description` method on the `Expectable` class to return the full description of the expectable including
   the optional description you setup using the `aka` method

#### Matching with a sequence of values

If you have the same "MatchResult" expression that you'd like to verify for different values you can write one of the following:

        // stop after the first failure
        ((_:Int) must be_>(2)).forall(Seq(3, 4, 5))
        forall(Seq(3, 4, 5)) ((_:Int) must be_>(2))
        // check only the elements defined for the partial function
        forallWhen(Seq(3, 10, 15)) { case a if a > 3 => a must be_>(5) }

        // try to match all values and collect the results
        ((_:Int) must be_>(2)).foreach(Seq(3, 4, 5))
        foreach(Seq(3, 4, 5)) ((_:Int) must be_>(2))
        // check only the elements defined for the partial function
        foreachWhen(Seq(3, 10, 15)) { case a if a > 3 => a must be_>(5) }

        // succeeds after the first success
        ((_:Int) must be_>(2)).atLeastOnce(Seq(3, 4, 5))
        atLeastOnce(Seq(3, 4, 5)) ((_:Int) must be_>(2))
        // check only the elements defined for the partial function
        atLeastOnceWhen(Seq(3, 4, 10)) { case a if a > 3 => a must be_>(5) }

### ScalaCheck properties

A clever way of creating expectations in ***specs2*** is to use the [ScalaCheck](http://code.google.com/p/scalacheck) library.

To declare ScalaCheck properties you first need to extend the `ScalaCheck` trait. Then you can pass functions to the `check` method and use the resulting block as your example body:

      "addition and multiplication are related" ! check { (a: Int) => a + a == 2 * a }

The function that is checked can either return:

      // a Boolean
      "addition and multiplication are related" ! check { (a: Int) => a + a == 2 * a }

      // a MatchResult
      "addition and multiplication are related" ! check { (a: Int) => a + a must_== 2 * a }

      // a Prop
      "addition and multiplication are related" ! check { (a: Int) => (a > 0) ==> (a + a must_== 2 * a) }

Note that if you pass functions using MatchResults you will get better failure messages so you are encouraged to do so.

#### Arbitrary instances

By default ScalaCheck uses `Arbitrary` instances taken from the surrounding example scope. However you'll certainly need to generate your own data from time to time. In that case you can create an `Arbitrary` instance and make sure it is in the scope of the function you're testing:

        // this arbitrary will be used for all the examples
        implicit def a = Arbitrary { for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b }

        "a simple property" ! ex1

         def ex1 = check((s: String) => s must contain("a") or contain("b"))

You can also be very specific if you want to use an `Arbitrary` instance only on one example. In that case, just replace the `check` method with the name of your `Arbitrary` instance:

        "a simple property"       ! ex1
        "a more complex property" ! ex2

        implicit def abStrings = Arbitrary { for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b }
        def ex1 = abStrings((s: String) => s must contain("a") or contain("b"))

        // use a tuple if there are several parameters to your function
        def ex2 = (abStrings, abStrings)((s1: String, s2: String) => s must contain("a") or contain("b"))

#### With Generators

ScalaCheck also allows to create `Prop`s directly with the `Prop.forAll` method accepting `Gen` instances:

        "a simple property"       ! ex1
        "a more complex property" ! ex2

        def abStrings = for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b

        def ex1 = forAll(abStrings) { (s: String) => s must contain("a") or contain("b") }
        def ex2 = forAll(abStrings, abStrings) { (s1: String, s2: String) => s must contain("a") or contain("b") }


#### Setting the ScalaCheck properties

ScalaCheck test generation can be tuned with a few properties. If you want to change the default settings, you have to use implicit values:

      implicit val params = set(minTestsOk -> 20) // use display instead of set to get additional console printing

It is also possible to specifically set the execution parameters on a given property:

      "this is a specific property" ! check { (a: Int, b: Int) =>
        (a + b) must_== (b + a)
      }.set(minTestsOk -> 200, workers -> 3)

The parameters you can modify are:

  * `minTestsOk`: minimum of tests which must be ok before the property is ok (default=100)
  * `maxDiscarded`: if the data generation discards too many values, then the property can't be proven (default=500)
  * `minSize`: minimum size for the "sized" data generators, like list generators (default=0)
  * `maxSize`: maximum size for the "sized" data generators (default=100)
  * `workers`: number of threads checking the property (default=1)

### Mock expectations

At the moment only the [Mockito](http://mockito.org) library is supported.

Mockito allows to specify stubbed values and to verify that some calls are expected on your objects. In order to use those functionalities, you need to extend the `org.specs2.mock.Mockito` trait:

      import org.specs2.mock._
      class MockitoSpec extends Specification { def is =

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
           def verify2 = there was no(m).get(0) // verify that the call never happened
         }
       }

##### Mocks creation and settings

Mockito offers the possibility to provide specific settings for the mock being created:

 * its name

        val m = mock[List[String]].as("list1")

 * "smart" return values

        val m = mock[List[String]].smart

 * specific return values

        val m = mock[List[String]].defaultReturn(10)

 * specific answers

        // a function InvocationOnMock => V is used in place of the org.mockito.stubbing.Answer type
    // for better conciseness
        val helloObject = (p1: InvocationOnMock) => "hello "+p1.toString
        val m = mock[List[String]].defaultAnswer(helloObject)

 * extra interfaces

        val m = mock[List[String]].extraInterface[Cloneable]
        val m = mock[List[String]].extraInterfaces(classesOf[Cloneable, Serializable])

Now, if you want to combine several of those settings together you need to call the `settings` method:

      val m = mock[List[String]].settings(name = "list1",
                                          defaultReturn = 10,
                                          extraInterfaces = classesOf[Cloneable, Serializable]))
      // or
      val m = mock[List[String]].settings(smart = true,
                                          extraInterface = classeOf[Cloneable]))

Finally, in case the Mockito library gets new settings, you can declare the following:

      val settings = org.mockito.Mockito.withSettings
      val m = mock[List[String]](settings)

##### Stubbing

Stubbing values is as simple as calling a method on the mock and declaring what should be returned or thrown:

      m.get(1) returns "one"
      m.get(2) throws new RuntimeException("forbidden")

You can specify different consecutive returned values by appending thenReturns or thenThrows:

      m.get(1) returns "one" thenReturns "two"
      m.get(2) throws new RuntimeException("forbidden") thenReturns "999"

##### Argument matchers
The built-in Mockito argument matchers can be used to specify the method arguments for stubbing:

      m.get(anyInt()) returns "element"
      m.get(999) must_== "element"

***specs2*** matchers can also be passed directly as arguments:

      m.get(===(123)) returns "one"

##### Callbacks

In some rare cases, it is necessary to have the return value depend on the parameters passed to the mocked method:

      m.get(anyInt) answers { i => "The parameter is " + i.toString }

The function passed to answers will be called with each parameter passed to the stubbed method:

     m.get(0)           // returns "The parameter is 0"
     m.get(1)           // the second call returns a different value: "The parameter is 1"

###### Parameters for the answers function

Because of the use of reflection the function passed to answers will receive only instances of the `java.lang.Object` type.

More precisely, it will:

 * pass the mock object if both the method has no parameters and the function has one parameter:
   `mock.size answers { mock => mock.hashCode }`
 * pass the parameter if both the method and the function have one parameter:
   `mock.get(0) answers ( i => i.toString )`
  * pass the parameter and the mock object if the method has 1 parameter and the function has 2:
    `mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString }`

In any other cases, if `f` is a function of 1 parameter, the array of the method parameters will be passed and if the function has 2 parameters, the second one will be the mock.

##### Verification

By default Mockito doesn't expect any method to be called. However if your writing interaction-based specifications you want to specify that some methods are indeed called:

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

It is also possible to add all verifications inside a block, when several mocks are involved:

      got {
        one(m).get(0)
        two(m).get(1)
      }

###### Order of calls

The order of method calls can be checked by creating calls and chaining them with then:

      val m1 = mock[List[String]]
      val m2 = mock[List[String]]

      m1.get(0)
      m1.get(0)
      m2.get(0)

      there was one(m1).get(0) then one(m1).get(1)

      // when several mocks are involved, the expected order must be specified as an implicit value
      implicit val order = inOrder(m1, m2)
      there was one(m1).get(0) then one(m2).get(0)

###### Spies

Spies can be used in order to do some "partial mocking" of real objects:

      val spiedList = spy(new LinkedList[String])

      // methods can be stubbed on a spy
      spiedList.size returns 100

      // other methods can also be used
      spiedList.add("one")
      spiedList.add("two")

      // and verification can happen on a spy
      there was one(spiedList).add("one")

However, working with spies can be tricky:

     // if the list is empty, this will throws an IndexOutOfBoundsException
     spiedList.get(0) returns "one"

As advised in the Mockito documentation, doReturn must be used in that case:

     doReturn("one").when(spiedList).get(0)

##### Functions and partial functions

Mocking with Scala functions or partial functions can be a bit tricky. You should especially watch out for:

 * functions as parameters. In that case you can try to create a ["function matcher"](https://groups.google.com/d/msg/specs2-users/R3DS_ZPe29w/WvmkfJqcq8IJ) or use [Borachio](http://borachio.com)

 * mocking partial functions. Auto-boxing might interfere, please have a look at [this discussion](https://groups.google.com/d/topic/specs2-users/_bK8lCCjZ4c/discussion)

### DataTables

DataTables are a very effective way of grouping several similar examples into one. For example, here is how to specify the addition of integers by providing one example on each row of a table:

      class DataTableSpec extends Specification with DataTables { def is =
        "adding integers should just work in scala"  ! e1

        def e1 =
          "a"   | "b" | "c" |                                   // the header of the table, with `|` separated strings
           2    !  2  !  4  |                                   // an example row
           1    !  1  !  2  |> {                                // the > operator to "execute" the table
           (a, b, c) =>  a + b must_== c                        // the expectation to check on each row
        }
      }

Note that there may be implicit definition conflicts when the first parameter of a row is a String. In that case you can use the `!!` operator to disambiguate (and `||` in the header for good visual balance).

DataTables are generally used to pack lots of expectations inside one example. In that case the table is only displayed when failing. If, on the other hand you want to display the table even when successful, to document your examples, you can omit the example description and inline the DataTable directly in the specification:

      class DataTableSpec extends Specification with DataTables { def is =

        "adding integers should just work in scala"  ^ {
          "a"   | "b" | "c" |
           2    !  2  !  4  |
           1    !  1  !  2  |> {
           (a, b, c) =>  a + b must_== c
        }
      }

This specification will be rendered as:

        adding integers should just work in scala
        +  a | b | c |
           2 | 2 | 4 |
           1 | 1 | 2 |

### Forms

Forms are a way to represent domain objects or service, and declare expected values in a tabular format. They are supposed to be used with the HtmlRunner to get human-readable documentation.

Forms can be designed as reusable pieces of specification where complex forms can be built out of simple ones.

""" ^
  "Here's " ~/ ("how to use Forms", new org.specs2.guide.Forms) ^
"""

### Reusing matchers outside of specs2

The ***specs2*** matchers are a well-delimited piece of functionality that you should be able to reuse in your own test framework. You can reuse the following traits:

 * `org.specs2.matcher.MustMatchers` (or `org.specs2.matcher.ShouldMatchers`) to write anything like `1 must be_==(1)` and
   get a `Result` back

 * You can also use the side-effecting version of that trait called `org.specs2.matcher.MustThrownMatchers` (or `ShouldThrownMatchers`).
   It throws a `FailureException` as soon as an expectation is failing. Those traits can also be used in a regular
   Specification if you have several expectations per example and if you don't want to chain them with `and`.

 * Finally, in a JUnit-like library you can use the `org.specs2.matcher.JUnitMustMatchers` trait which throws
   `AssertionFailureError`s

#### Without any dependency on specs2

The [Testing](https://github.com/spray/spray/wiki/Testing) page of the ***spray*** project explains how you can define a testing trait in your library which can be used with specs2 or scalatest or any framework defining the following methods:

   * `fail(msg: String): Nothing`
   * `failure(msg: String): Nothing`

In specs2, those 2 methods are defined by the `org.specs2.matcher.ThrownMessages` trait

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
    implicit val params = set(minTestsOk -> 20)

    def is = "Scalacheck".title ^
    "addition and multiplication are related" ! Prop.forAll { (a: Int) => a + a == 2 * a }              ^
    "addition and multiplication are related" ! check { (a: Int) => a + a == 2 * a }                    ^
    "addition and multiplication are related" ! check { (a: Int) => a + a must_== 2 * a }               ^
    "addition and multiplication are related" ! check { (a: Int) => (a > 0) ==> (a + a must_== 2 * a) } ^
    "this is a specific property" ! check { (a: Int, b: Int) =>
      (a + b) must_== (b + a)
    }.set(minTestsOk -> 200, workers -> 1)                                                              ^
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
class JsonExamples extends Specification {
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
}

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
}
