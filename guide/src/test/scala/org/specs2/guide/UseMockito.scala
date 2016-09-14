package org.specs2
package guide

import java.util.LinkedList
import org.mockito.invocation.InvocationOnMock
import org.specs2.mock._
import scala.concurrent.duration._

object UseMockito extends UserGuidePage with Mockito { def is = "Mockito".title ^ s2"""

[Mockito](https://github.com/mockito/mockito) allows to specify stubbed values and to verify that some calls have been made to your objects. In order to use those features, you need to extend the `org.specs2.mock.Mockito` trait:${snippet{

class MockitoSpec extends Specification with Mockito { def is = s2"""

A java list can be mocked
  You can make it return a stubbed value        ${c().stub}
  You can verify that a method was called       ${c().verify}
  You can verify that a method was not called   ${c().verify2}

"""

  case class c() {
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
}}

### Creation and settings

Mockito offers the possibility to provide specific settings for the mock being created:

 * a name

${snippet{ val m = mock[List[String]].as("list1") }}

 * ["smart" return values](https://code.google.com/p/specs/wiki/UsingMockito#Smart_mocks)

${snippet{ val m = mock[List[String]].smart }}

 * `verbose` to enable Mockito's verbose logging

`val m = mock[List[String]].verbose`

 * specific return values

${snippet{ val m = mock[List[String]].defaultReturn(10) }}

 * specific answers ${snippet{
// a function InvocationOnMock => V is used in place of the org.mockito.stubbing.Answer type for better conciseness
val helloObject = (p1: InvocationOnMock) => "hello "+p1.toString
val m = mock[List[String]].defaultAnswer(helloObject)
}}

 * extra interfaces ${snippet{
val m1 = mock[List[String]].extraInterface[Cloneable]
val m2 = mock[List[String]].extraInterfaces[Cloneable, Serializable]
}}

Now, if you want to combine several of those settings together you need to call the `settings` method: ${snippet{
val m1 = mock[List[String]].settings(name = "list1",
  defaultReturn = 10,
  extraInterfaces = classesOf[Cloneable, Serializable])
// or
val m2 = mock[List[String]].settings(smart = true,
  extraInterface = classOf[Cloneable])
}}

Finally, you can pass a `org.mockito.MockSettings` object directly to the `mock` method: ${snippet{
val settings = org.mockito.Mockito.withSettings
val m = mock[List[String]](settings)
}}

### Stubbing

Stubbing values is as simple as calling a method on the mock and declaring what should be returned or thrown: ${snippet{
m.get(1) returns "one"
m.get(2) throws new RuntimeException("forbidden")
}}

You can specify different consecutive returned values by appending `thenReturns` or `thenThrows`: ${snippet{
m.get(1) returns "one" thenReturns "two"
m.get(2) throws new RuntimeException("forbidden") thenReturns "999"
}}

### Mocking and Stubbing at the same time

It is also possible to create a mock while stubbing one of its methods, provided that you declare the type of the expected mock: ${snippet{
val mocked: java.util.List[String] = mock[java.util.List[String]].contains("o") returns true
mocked.contains("o") must beTrue
}}

### With matchers

The built-in Mockito argument matchers can be used to specify the method arguments for stubbing: ${snippet{
m.get(org.mockito.Matchers.anyInt()) returns "element"
m.get(999) must_== "element"
}}

$specs2 matchers can also be passed directly as arguments: ${snippet{
m.get(===(123)) returns "one"
}}

*Note*: the call above works because there is an implicit method `argThat` which transforms a $specs2 `Matcher[T]` into a Hamcrest one and in turn call Mockito's `org.mockito.Matchers.argThat` method to register the Hamcrest matcher. However [sometimes](https://groups.google.com/d/forum/specs2-users/_slOZQoICzU/DF-ZQCq_GmkJ) the implicit conversion is not called and you have to explicitly call the `argThat` method like so: ${snippet{
m.get(argThat(===(123))) returns "one"
}}

### Callbacks

In some rare cases, it is necessary to have the return value depend on the parameters passed to the mocked method: ${snippet{
m.get(anyInt) answers { i => "The parameter is " + i.toString }
}}

The function passed to `answers` will be called with each parameter passed to the stubbed method: ${snippet{
m.get(0)    // returns "The parameter is 0"
m.get(1)    // the second call returns a different value: "The parameter is 1"
}}

To use specific a type of argument: ${snippet{
m.get(anyInt) answers { _ match { case i: Int => (i + 1).toString } }
}}

Or more concisely: ${snippet{
m.get(anyInt) responds { case i: Int => (i + 1).toString }
}}

#### Parameters for the `answers` function

Because of the use of reflection the function passed to answers will receive only instances of the `java.lang.Object` type.

More precisely, it will:

 * pass the mock object if both the method has no parameters and the function has one parameter:
`mock.size answers { mock => mock.hashCode }`
 * pass the parameter if both the method and the function have one parameter:
`mock.get(0) answers { i => i.toString }`
 * pass the parameter and the mock object if the method has 1 parameter and the function has 2:
`mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString }`

In any other cases, if `f` is a function of 1 parameter, the array of the method parameters will be passed and if the function has 2 parameters, the second one will be the mock.

### Verification

By default Mockito doesn't expect any method to be called. However if you are writing interaction-based specifications you want to specify that some methods are indeed called: ${snippet{
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

#### Order of calls

The order of method calls can be checked by creating calls and chaining them with `andThen`: ${snippet{
val m1 = mock[java.util.List[String]]

m1.get(0)
m1.get(1)

there was one(m1).get(0) andThen one(m1).get(1)
}}

when several mocks are involved, the expected order must be specified as an implicit value: ${snippet{
val m1 = mock[java.util.List[String]]
val m2 = mock[java.util.List[String]]
val m3 = mock[java.util.List[String]]

// the order of mock objects doesn't matter here
implicit val order = inOrder(m1, m3, m2)

m1.get(1); m2.get(2); m3.get(3)

there was one(m1).get(1) andThen one(m2).get(2) andThen one(m3).get(3)
}}

#### Ignoring stubs

When specifying the behavior of an object in relation to others you may want to verify that some mocks have been called as collaborators and you don't really want to specify what happens to other mocks because they are just playing the role of stubs.

In this case the `ignoreStubs` method can be used: ${snippet{
val (stub1, stub2) = (mock[AStub], mock[AStub])
there were noMoreCallsTo(ignoreStubs(stub1, stub2))
}}

This method is also available with the `inOrder` method: ${snippet{
// 8<--
val (list1, list2) = ("", "")
// 8<--
implicit val order = inOrder(ignoreStubs(list1, list2))
}}

For more documentation about this Mockito functionality, please read [here](http://docs.mockito.googlecode.com/hg/1.9.0/org/mockito/Mockito.html#25).

#### Spies

Spies can be used to do "partial mocking" of real objects: ${snippet{
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

As advised in the Mockito documentation, `doReturn` must be used in that case: ${snippet{
// 8<--
val spiedList = spy(new LinkedList[String])
// 8<--
org.mockito.Mockito.doReturn("one").when(spiedList).get(0)
}}

#### Functions / PartialFunctions

It is possible to verify method calls where parameters are functions by specifying how the passed function will react to a given set of arguments. Given the following mock:${snippet{
trait Amount {
  // a method showing an amount precision
  def show(display: (Double, Int) => String) = ???
}

val amount = mock[Amount]
}}

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

#### Auto-boxing

Auto-boxing might interfere with the mocking of PartialFunctions. Please have a look at [this](https://groups.google.com/d/topic/specs2-users/_bK8lCCjZ4c/discussion) for a discussion.

#### Byname

Byname parameters can be verified but this will not work if the $specs2 jar is not put first on the classpath, before the mockito jar. Indeed $specs2 redefines a Mockito class for intercepting method calls so that byname parameters are properly handled.
"""

  trait Amount { def show(display: Function2[Double, Int, String]) = "" }
  trait AStub
  def m: java.util.List[String] = mock[java.util.List[String]]

}
