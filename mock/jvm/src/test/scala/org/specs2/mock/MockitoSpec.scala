package org.specs2
package mock

import java.util

import specification.core.{Env, Fragment}
import specification.process.DefaultExecutor
import control.Exceptions._
import org.hamcrest.core.IsNull
import org.mockito.Mockito.withSettings
import org.mockito.invocation._
import matcher._
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import MatchersImplicits._
import execute._
import ActionMatchers._
import fp.syntax._

class MockitoSpec extends Spec with Mockito with ResultMatchers {  def is = s2"""

 Mockito is a Java library for mocking.

 The following samples are taken from the main documentation which can be found here:
 http://mockito.googlecode.com/svn/tags/latest/javadoc/org/mockito/Mockito.html

 CREATION
 ========

 Mocks can be created
  with a name $creation1
  with a default return value $creation2
  with a name and default return value $creation3
  with a default answer $creation4
  with settings $creation5

 VERIFICATION
 ============

 When a mock is created with the mock method
  it is possible to call methods on the mock $verification1
  it is possible to verify that a method has been called $verification2
  if one method has not been called on a mock there will be a failure $verification3
  it is possible to check that no calls have been made $verification4
  null values can be checked with beNull $verification5

  it is possible to pass byname parameters $verification6
    with several byname parameters $verification7
    with 2 parameter lists and byname parameters $verification8
  it is possible to check byname parameters $verification9
    with several byname parameters $verification10
    with mixed byname parameter and byvalue parameter $verification11
    with 2 parameter lists and byname parameters $verification12

  it is possible to check a function parameter
    with one argument $verification13
    with one argument and a matcher for the return value $verification14
    with n arguments $verification15
    with n arguments and a matcher for the return value $verification16
    as being anything $verification17
    with Nothing as the return type $verification18
    with Any as the return type $verification19

  it is possible to check a partial function parameter
    with n arguments $verification20
    with n arguments and a matcher for the return value $verification21
    as being anything $verification22
    when the argument is not defined $verification23

  it is possible to verify a function with repeated parameters $verification24
  it is possible to specify a timeout for the call $verification25
  it doesn't match maps and functions as equal $verification26
  spies must not be checked for matchers when called for real $verification27

STUBS
=====

 It is also possible to return a specific value from a mocked method
  then when the mocked method is called, the same values will be returned $stub1
  different successive values can even be returned $stub2
  a value can be returned when a parameter of the method matches
    a hamcrest matcher $stub3
    a specs2 matcher $stub4
    with a subtype matcher $stub5
    a Set $stub6
    a List $stub7

 It is also possible to throw an exception from a mocked method
  then when the mocked method is called, the exception will be thrown $stub8
  different successive exceptions can even be thrown $stub9

 A mock can be created and stubbed at the same time $stub10

 NUMBER OF CALLS
 ===============

 The number of calls to a mocked method can be checked
  if the mocked method has been called once $callsNb1
  if the mocked method has been called twice $callsNb2
  if the mocked method has been called exactly n times $callsNb3
  if the mocked method has been called atLeast n times $callsNb4
  if the mocked method has been called atMost n times $callsNb5
  if the mocked method has never been called $callsNb6
  if the verification throws an exception, it will be reported as an Error $callsNb7
  if the mocked method has not been called after some calls $callsNb8
  if the mocked method has not been called after some calls - ignoring stubs $callsNb9

 ORDER OF CALLS
 ==============

 The order of calls to a mocked method can be checked
  with 2 calls that were in order $order1
  with 2 calls that were in order - ignoring stubbed methods $order2
  with 2 calls that were not in order - on the same mock $order3
  with 2 calls that were not in order - on the same mock, with thrown expectations $order4
  with 2 calls that were not in order - on different mocks $order5
  with 3 calls that were not in order $order6

 ANSWERS & PARAMETERS CAPTURE
 ============================

  Answers can be created to control the returned a value $answer1
  Answers can use the method's parameters passed as an array $answer2
  Answers can use the mock instance as the second parameter $answer3
  Answers can use the mock instance, even when the method has 0 parameters $answer4
  Answers can use the all the invocation parameters $answer5
  Answers can use the all the invocation parameters as an array $answer6

  A parameter can be captured in order to check its value $capture1
  A parameter can be captured in order to check its successive values $capture2

 OTHER CONTEXTS
 ==============

 The Mockito trait is reusable in other contexts
  in mutable specs $context1
  with an in order call $context2

 MATCHERS
 ========

 Various mockito matchers can be used $matcher1
 Matching with any $matcher2

${step(env)}
"""

  trait ListOf[T] {
    def add(t: T): Boolean
    def add(i: Int, t: T): Unit
    def set(i: Int, t: T): T
    def get(i: Int): T
    def size(): Int
    def clear(): ListOf[T]
    def contains(t: Any): Boolean
  }

  lazy val env = Env()

  def creation1 = {
    val list = mock[ListOf[String]].as("list1")
    (there was one(list).add("one")).message must contain("list1.add(\"one\")")
  }

  def creation2 = {
    val list = mock[ListOf[String]].settings(defaultReturn = 10)
    list.size must_== 10
  }

  def creation3 = {
    val list = mock[ListOf[String]].settings(name = "list1", defaultReturn = 10, extraInterfaces = classesOf[Cloneable, Serializable])
    (list.size must_== 10) and
    ((there was one(list).add("one")).message must contain("list1.add(\"one\")"))
  }

  def creation4 = {
    val list = mock[ListOf[String]].defaultAnswer((p1: InvocationOnMock) => "hello")
    list.get(0) must_== "hello"
  }

  def creation5 = {
    val list = mock[ListOf[String]](withSettings.name("list1"))
    (there was one(list).add("one")).message must contain("list1.add(\"one\")")
  }

  def verification1 = {
    val list = mock[ListOf[String]]
    list.add("one")
    success
  }

  def verification2 = {
    val list = mock[ListOf[String]]
    list.add("one")
    there was one(list).add("one")
  }

  def verification3 = {
    val list = mock[ListOf[String]]
    (there was one(list).add("one")).message must startWith("The mock was not called as expected")
  }

  def verification4 = {
    val list = mock[ListOf[String]]
    there were noCallsTo(list)
  }

  def verification5 = {
    val list = mock[ListOf[String]]
    list.add(3, null: String)
    there was one(list).add(be_>(0), beNull[String])
  }

  def verification6 = {
    val byname = mock[ByName]
    byname.call(10)
    there was one(byname).call(10)
  }

  def verification7 = {
    val byname = mock[ByName]
    byname.add(1, 2)
    there was one(byname).add(1, 2)
  }

  def verification8 = {
    val byname = mock[ByName]
    byname.mult(1)(2)
    there was one(byname).mult(1)(2)
  }

  def verification9 = {
    val byname = mock[ByName]
    byname.call(10)
    there was one(byname).call(be_>(5))
  }

  def verification10 = {
    val byname = mock[ByName]
    byname.add(1, 2)
    there was one(byname).add(anyInt, anyInt)
  }

  def verification11 = {
    val byname = mock[ByName]
    byname.min(2, 1)
    there was one(byname).min(anyInt, anyInt)
  }

  def verification12 = {
    val byname = mock[ByName]
    byname.mult(1)(2)
    there was one(byname).mult(anyInt)(anyInt)
  }

  def verification13 = {
    val function1 = mock[WithFunction1]
    function1.call((_:Int).toString)
    there was one(function1).call(1 -> "1")
  }

  def verification14 = {
    val function1 = mock[WithFunction1]
    function1.call((_:Int).toString)
    (there was one(function1).call(1 -> startWith("1"))) and
    ((there was one(function1).call(1 -> startWith("2"))).message must contain("1 doesn't start with '2'"))
  }

  def verification15 = {
    val function2 = mock[WithFunction2]
    function2.call((i:Int, d: Double) => (i + d).toString)
    there was one(function2).call((1, 3.0) -> "4.0")
  }

  def verification16 = {
    val function2 = mock[WithFunction2]
    function2.call((i:Int, d: Double) => (i + d).toString)
    there was one(function2).call((1, 3.0) -> haveSize[String](3))
  }
  def verification17 = {
    val function2 = mock[WithFunction2]
    function2.call((i:Int, d: Double) => (i + d).toString)
    there was one(function2).call(anyFunction2)
  }

  def verification18 = {
    val functionNothing = mock[WithFunctionNothing]
    functionNothing.call((i:Int) => throw new Exception)
    there was one(functionNothing).call(anyFunction1)
  }

  def verification19 = {
    val functionAny = mock[WithFunctionAny]
    functionAny.call(() => throw new Exception)
    there was one(functionAny).call(any[() => Any])
  }

  def verification20 = {
    val partial = mock[WithPartialFunction]
    partial.call { case (i:Int, d: Double) => (i + d).toString }
    there was one(partial).call((1, 3.0) -> "4.0")
  }

  def verification21 = {
    val partial = mock[WithPartialFunction]
    partial.call { case (i:Int, d: Double) => (i + d).toString }
    there was one(partial).call((1, 3.0) -> haveSize[String](3))
  }

  def verification22 = {
    val partial = mock[WithPartialFunction]
    partial.call { case (i:Int, d: Double) => (i + d).toString }
    there was one(partial).call(anyPartialFunction)
  }

  def verification23 = {
    val partial = mock[WithPartialFunction]
    partial.call { case (i:Int, d: Double) if i > 10 => (i + d).toString }
    (there was one(partial).call((1, 3.0) -> "4.0")).message must contain("a PartialFunction defined for (1,3.0)")
  }

  def verification24 = {
    val repeated = mock[WithRepeatedParams]
    repeated.call(1, 2, 3)
    (there was one(repeated).call(1, 2, 3)) and
    ((there was one(repeated).call(1, 2)).message must contain("(1, 2)"))
  }

  def verification25 = {
    val takesSometime = mock[TakesSometime]
    scala.concurrent.Future { Thread.sleep(200); takesSometime.call(10) }
    ((there was after(10.millis).one(takesSometime).call(10)).message must contain("Wanted but not invoked")) and
    (there was after(300.millis).one(takesSometime).call(10))
  }

  def verification26 = {
    val functionInt = mock[WithFunctionInt]
    functionInt.call((i: Int) => i + 2)
    (there was one(functionInt).call(Map(1 -> 2))).message must contain("Argument(s) are different")
  }

  def verification27 = {
    val foo = mock[FooComponent]
    val controller = spy(new TestController(foo))
    foo.getBar(1) returns 1
    // controller is a spy. Calling 'test' for real must not re-evaluate
    // the arguments, hence make a mock call, to register matchers
    controller.test(1)
    there were 1.times(foo).getBar(1)
  }

  def stub1 = {
    val list = mockAs[ListOf[String]]("list")
    list.add("one") returns true
    list.add("one") must_== true
  }

  def stub2 = {
    val list = mockAs[ListOf[String]]("list")
    list.add("one") returns (true, false, true)
    (list.add("one"), list.add("one"), list.add("one")) must_== ((true, false, true))
  }

  def stub3 = {
    val list = mockAs[ListOf[String]]("list")
    list.contains(anArgThat(new IsNull[String])) returns true
    list.contains(null) must_== true
  }

  def stub4 = {
    val list = mockAs[ListOf[String]]("list")
    list.contains(beMatching(".*o")) returns true
    list.contains("o") must_== true
  }

  def stub5 = {
    trait Vet { def treat(p: Pet) = true }
    trait Pet
    case class Dog() extends Pet
    case class Cat() extends Pet
    val vet = mock[Vet]
    vet.treat(Cat())
    def isDog: Matcher[Dog] = (d: Dog) => (true, "ok", "ko")
    (there was one(vet).treat(isDog)) must not(throwA[ClassCastException])
  }

  def stub6 = {
    val list = mockAs[ListOf[String]]("list")
    list.contains(Set(1)) returns true
    list.contains(Set(1)) must_== true
    list.contains(Set(2)) must_== false
  }

  def stub7 = {
    val list = mockAs[ListOf[String]]("list")
    list.contains(List(1)) returns true
    list.contains(List(1)) must_== true
    list.contains(List(2)) must_== false
  }

  def stub8 = {
    val list = mockAs[ListOf[String]]("list")
    list.clear() throws new RuntimeException
    list.clear()
  } must throwA[RuntimeException]

  def stub9 = {
    val list = mockAs[ListOf[String]]("list")
    list.clear() throws (new RuntimeException, new IllegalArgumentException)
    tryo(list.clear())
    list.clear()
  } must throwAn[IllegalArgumentException]

  def stub10 = {
    val mocked: ListOf[String] = mock[ListOf[String]].contains("o") returns true
    mocked.contains("o") must beTrue
  }

  def callList = {
    val list = mock[ListOf[String]]
    list.add("one")
    1 to 2 foreach { i => list.add("two") }
    list
  }

  def callsNb1 = {
    val list = callList
    got { one(list).add("one") } // equivalent to 'there was one(list).add("one")'
  }

  def callsNb2 = {
    val list = callList
    there were two(list).add("two")
  }

  def callsNb3 = {
    val list = callList
    there was atLeast(1)(list).add("two")
  }

  def callsNb4 = {
    val list = callList
    there was exactly(2)(list).add("two")
  }

  def callsNb5 = {
    val list = callList
    there were atMost(2)(list).add("two")
  }

  def callsNb6 = {
    val list = callList
    there was no(list).add("four")
  }

  def callsNb7 = {
    val list = callList
    val cause = new Exception("cause")
    val e = new Exception("error", cause)
    (there was no(list).add(be_==={throw e; "four"})) must throwAn[Exception]
  }

  def callsNb8 = {
    val list = callList
    there was one(list).add("one")
    there were two(list).add("two")
    there were noMoreCallsTo(list)
  }

  def callsNb9 = {
    val list3 = mock[ListOf[String]]
    val list4 = mock[ListOf[String]]

    list3.contains("3") returns false
    list4.contains("4") returns false
    list3.add("one")
    list4.add("one"); list4.add("one")
    list3.contains("3")
    list4.contains("4")

    there was one(list3).add("one")
    there were two(list4).add("one")
    there were noMoreCallsTo(ignoreStubs(list3, list4))
  }

  def order1 = {
    val list1 = mock[ListOf[String]]
    val list2 = mock[ListOf[String]]
    list1.get(0)
    list2.get(0)
    implicit val order = inOrder(list1, list2)
    (there was one(list1).get(0) andThen
      one(list2).get(0)).message must_== "The mock was called as expected"
  }

  def order2 = {
    val list1 = mock[ListOf[String]]
    val list2 = mock[ListOf[String]]
      list1.get(1) returns "1"
    // there is an out of order call but to a stubbed method
    list1.get(1)
    list1.get(0)
    list2.get(0)
    implicit val order = inOrder(ignoreStubs(list1, list2))
    (there was one(list1).get(0) andThen
      one(list2).get(0)).message must_== "The mock was called as expected"
  }

  def order3 = {
    val list1 = mock[ListOf[String]]

    list1.get(0); list1.get(1)
    implicit val order = inOrder(list1)
    val result = there was one(list1).get(1) andThen
      one(list1).get(0)
    result.message must startWith("The mock was not called as expected")
  }

  def order4 = {
    val list1 = mock[ListOf[String]]

    list1.get(0); list1.get(1)
    implicit val order = inOrder(list1)
    var result: Result = success
    new Mockito with ThrownExpectations {
      result = AsResult(there was one(list1).get(1) andThen
        one(list1).get(0))
    }
    result.message must startWith("The mock was not called as expected")
  }

  def order5 = {
    val list1 = mock[ListOf[String]]
    val list2 = mock[ListOf[String]]

    list1.get(0)
    list2.get(0)
    implicit val order = inOrder(list1, list2)
    (there was one(list2)(order).get(0) andThen
      one(list1)(order).get(0)).message must startWith("The mock was not called as expected")
  }

  def order6 = {
    val list1 = mock[ListOf[String]]

    list1.get(0); list1.size; list1.get(0); list1.size
    implicit val order = inOrder(list1)
    val result = there was one(list1).get(0) andThen
      one(list1).size() andThen
      no(list1) .get(0) andThen
      one(list1).size()
    result.message must startWith("The mock was not called as expected")
  }

  def answer1 = {
    val list = mock[ListOf[String]]
    list.get(anyInt) answers { i: Any => "The parameter is " + i.toString}
    list.get(2) must_== "The parameter is 2"
  }

  def answer2 = {
    val list = mock[ListOf[String]]
    list.get(anyInt) responds { case i: Int => (i + 1).toString }
    list.get(1) must_== "2"
    list.get(5) must_== "6"
  }

  def answer3 = {
    val list = mock[ListOf[String]]
    list.set(anyInt, anyString) answers { i: Any => "The parameters are " + (i.asInstanceOf[Array[_]].mkString("(",",",")")) }
    list.set(1,"foo") must_== "The parameters are (1,foo)"
  }

  def answer4 = {
    val list = mock[ListOf[String]].as("list")
    list.get(anyInt) answers { (i, m) => "The parameters are " + (i.asInstanceOf[Array[_]].mkString -> m)}
    list.get(1) must_== "The parameters are (1,list)"
  }

  def answer5 = {
    val list = mock[ListOf[String]].as("list")
    list.size answers { m: Any => m.toString.length }
    list.size must_== 4
  }

  def answer6 = {
    val list = mock[ListOf[String]]
    list.get(anyInt) answers { is: Array[AnyRef] => "The parameters are "+is.mkString("[", ",", "]") }
    list.get(2) must_== "The parameters are [2]"
  }

  def capture1 = {
    val list = mock[ListOf[String]]
    list.get(1)
    val c = capture[Int]
    there was one(list).get(c)
    c.value must_== 1
  }

  def capture2 = {
    val list = mock[ListOf[String]]
    list.get(1)
    list.get(2)
    val c = capture[Int]
    there was two(list).get(c)
    c.values.toString === "[1, 2]"
  }

  def context1 = {
    val s = new org.specs2.mutable.Specification with Mockito {
      val list = mock[ListOf[String]]
      "ex1" in {
        list.add("one")
        there was one(list).add("two")
        1 must_== 1 // to check if the previous expectation really fails
      }
    }
    DefaultExecutor.runSpec(s.is, env).filter(Fragment.isExample).traverse(_.executionResult.map(_.isSuccess)) must beOk(contain(false))
  }

  def context2 = {
    val s = new org.specs2.mutable.Specification with Mockito {
      "ex1" in new specification.Scope {
        val (list1, list2) = (mock[ListOf[String]], mock[ListOf[String]])
        list1.add("two"); list2.add("one")
        implicit val order = inOrder(list1, list2)
        there was one(list2).add("two") andThen one(list1).add("one")
      }
    }
    DefaultExecutor.runSpec(s.is, env).filter(Fragment.isExample).traverse(_.executionResult.map(_.isSuccess)) must beOk(contain(false))
  }

  trait M {
    def javaList[T](a: java.util.List[T]): Unit
    def javaSet[T](a: java.util.Set[T]): Unit
    def javaCollection[T](a: java.util.Collection[T]): Unit
    def javaMap[K, V](a: java.util.Map[K, V]): Unit
    def List[T](a: List[T]): Unit
    def Set[T](a: Set[T]): Unit
    def Traversable[T](a: Traversable[T]): Unit
    def Map[K, V](a: Map[K, V]): Unit
    def varargs[T](ts: T*): Unit
    def method(a1: A, b: Boolean): Int
  }

  trait A
  class B extends A { override def toString = "B" }
  class C extends A { override def toString = "C" }

  def matcher1 = {
    val m = mock[M]
    m.javaList(new util.ArrayList[Int])
    m.javaSet(new util.HashSet[Int])
    m.javaCollection(new util.ArrayList[Int])
    m.javaMap(new util.HashMap[Int, String])
    m.List(List[Int]())
    m.Set(Set[Int]())
    m.Traversable(List[Int]())
    m.Map(Map[Int, String]())
    m.varargs(1, 2)

    there was one(m).javaList(anyJavaList)
    there was one(m).javaList(anyJavaListOf[Int])
    there was one(m).javaSet(anyJavaSet)
    there was one(m).javaSet(anyJavaSetOf[Int])
    there was one(m).javaCollection(anyJavaCollection)
    there was one(m).javaCollection(anyJavaCollectionOf[Int])
    there was one(m).javaMap(anyJavaMap)
    there was one(m).javaMap(anyJavaMapOf[Int, String])
    there was one(m).List(anyList)
    there was one(m).List(anyListOf[Int])
    there was one(m).Set(anySet)
    there was one(m).Set(anySetOf[Int])
    there was one(m).Traversable(anyTraversable)
    there was one(m).Traversable(anyTraversableOf[Int])
    there was one(m).Map(anyMap)
    there was one(m).Map(anyMapOf[Int, String])
    there was one(m).varargs(anyVarArg[Int])
  }

  def matcher2 = {
    val m = mock[M]
    m.method(any[B], any[Boolean]) returns 1
    m.method(anyObject, anyObject) returns 1
    m.method(new B, true)

    there was one(m).method(any[B], any[Boolean])
    there was one(m).method(any(), anyObject)
  }

  /**
   * HELPERS
   */
  trait ByName {
    def call(i: =>Int) = i
    def add(i: =>Int, j: =>Int) = i + j
    def min(i: Int, j: =>Int) = i - j
    def mult(i: =>Int)(j: =>Int) = i * j
  }

  trait WithFunction1 { def call(f: Int => String) = f(0) }
  trait WithFunction2 { def call(f: (Int, Double) => String) = f(1, 2.0) }
  trait WithPartialFunction { def call(f: PartialFunction[(Int, Double), String]) = f.apply((1, 2.0)) }
  trait TakesSometime { def call(i: Int) = i }
  trait WithRepeatedParams { def call[T](params: T*) = params.toString }
  trait WithFunctionNothing { def call(f: Int => Nothing) = 1 }
  trait WithFunctionAny { def call(f: () => Any) = 1 }
  trait WithFunctionInt { def call(f: Int => Any) = 1 }

  // this example comes from #428
  class FooComponent { def getBar(id: Int): Int = id }

  class TestController(foo: FooComponent) {
    def async(f: =>Int): Int = f
    def test(id: Int) = async { foo.getBar(id) }
  }
}
