package org.specs2
package mock

import java.util

import specification.core.{Env, Fragment}
import specification.process.DefaultExecutor
import specification._
import control.Exceptions._
import org.hamcrest.core.IsNull
import org.mockito.Mockito.withSettings
import org.mockito.invocation._
import matcher._
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global
import MatchersImplicits._
import ReturnsSyntax._
import execute._

class MockitoSpec extends script.Spec with Mockito with ResultMatchers with Groups {  def is = s2"""

 Mockito is a Java library for mocking.

 The following samples are taken from the main documentation which can be found here:
 http://mockito.googlecode.com/svn/tags/latest/javadoc/org/mockito/Mockito.html
                                                                                                                       
 CREATION
 ========

 Mocks can be created
   + with a name
   + with a default return value
   + with a name and default return value
   + with a default answer
   + with settings

 VERIFICATION
 ============

 When a mock is created with the mock method
   + it is possible to call methods on the mock
   + it is possible to verify that a method has been called
   + if one method has not been called on a mock there will be a failure
   + it is possible to check that no calls have been made
   + null values can be checked with beNull
   + it is possible to pass byname parameters
     + with several byname parameters
     + with 2 parameter lists and byname parameters
   + it is possible to check byname parameters
     + with several byname parameters
     + with mixed byname parameter and byvalue parameter
     + with 2 parameter lists and byname parameters
   it is possible to check a function parameter
     + with one argument
     + with one argument and a matcher for the return value
     + with n arguments
     + with n arguments and a matcher for the return value
     + as being anything
     + with Nothing as the return type
     + with Any as the return type

   it is possible to check a partial function parameter
     + with n arguments
     + with n arguments and a matcher for the return value
     + as being anything
     + when the argument is not defined

   + it is possible to verify a function with repeated parameters
   + it is possible to specify a timeout for the call
   + it doesn't match maps and functions as equal
   + spies must not be checked for matchers when called for real

STUBS
=====

 It is also possible to return a specific value from a mocked method
   + then when the mocked method is called, the same values will be returned
   + different successive values can even be returned
   a value can be returned when a parameter of the method matches
     + a hamcrest matcher
     + a specs2 matcher
     + with a subtype matcher
     + a Set
     + a List

 It is also possible to throw an exception from a mocked method
   + then when the mocked method is called, the exception will be thrown
   + different successive exceptions can even be thrown

 + A mock can be created and stubbed at the same time

 NUMBER OF CALLS
 ===============

 The number of calls to a mocked method can be checked
   + if the mocked method has been called once
   + if the mocked method has been called twice
   + if the mocked method has been called exactly n times
   + if the mocked method has been called atLeast n times
   + if the mocked method has been called atMost n times
   + if the mocked method has never been called
   + if the verification throws an exception, it will be reported as an Error
   + if the mocked method has not been called after some calls
   + if the mocked method has not been called after some calls - ignoring stubs

 ORDER OF CALLS
 ==============

 The order of calls to a mocked method can be checked
   + with 2 calls that were in order
   + with 2 calls that were in order - ignoring stubbed methods
   + with 2 calls that were not in order - on the same mock
   + with 2 calls that were not in order - on the same mock, with thrown expectations
   + with 2 calls that were not in order - on different mocks
   + with 3 calls that were not in order

 ANSWERS & PARAMETERS CAPTURE
 ============================

 + Answers can be created to control the returned a value
 + Answers can use the method's parameters passed as an array
 + Answers can use the mock instance as the second parameter
 + Answers can use the mock instance, even when the method has 0 parameters

 + A parameter can be captured in order to check its value
 + A parameter can be captured in order to check its successive values

 OTHER CONTEXTS
 ==============

 The Mockito trait is reusable in other contexts
   + in mutable specs
   + with an in order call

 MATCHERS
 ========

 + Various mockito matchers can be used

                                                                                                                        """
    
  "creation" - new group {
    eg := {
      val list = mock[java.util.List[String]].as("list1")
      (there was one(list).add("one")).message must contain("list1.add(\"one\")")
    }
    eg := {
      val list = mock[java.util.List[String]].settings(defaultReturn = 10)
      list.size must_== 10
    }
    eg := {
      val list = mock[java.util.List[String]].settings(name = "list1", defaultReturn = 10, extraInterfaces = classesOf[Cloneable, Serializable])
      (list.size must_== 10) and 
      ((there was one(list).add("one")).message must contain("list1.add(\"one\")"))
    }
    eg := {
      val list = mock[java.util.List[String]].defaultAnswer((p1: InvocationOnMock) => "hello")
      list.get(0) must_== "hello" 
    }
    eg := {
      val list = mock[java.util.List[String]](withSettings.name("list1"))
      (there was one(list).add("one")).message must contain("list1.add(\"one\")")
    }
  }

  "verification" - new group with list {
    eg := { list.add("one"); success }
    eg := {
      list.add("one")
      there was one(list).add("one")
    }
    eg := (there was one(list).add("one")).message must startWith("The mock was not called as expected")

    eg := there were noCallsTo(list)

    eg := {
      list.add(3, null: String)
      there was one(list).add(be_>(0), beNull[String])
    }

    eg := {
      byname.call(10)
      there was one(byname).call(10)
    }
    eg := {
      byname.add(1, 2)
      there was one(byname).add(1, 2)
    }
    eg := {
      byname.mult(1)(2)
      there was one(byname).mult(1)(2)
    }
    eg := {
      byname.call(10)
      there was one(byname).call(be_>(5))
    }
    eg := {
      byname.add(1, 2)
      there was one(byname).add(anyInt, anyInt)
    }
    eg := {
      byname.min(2, 1)
      there was one(byname).min(anyInt, anyInt)
    }
    eg := {
      byname.mult(1)(2)
      there was one(byname).mult(anyInt)(anyInt)
    }

    eg := {
      function1.call((_:Int).toString)
      there was one(function1).call(1 -> "1")
    }
    eg := {
      function1.call((_:Int).toString)
      (there was one(function1).call(1 -> startWith("1"))) and
      ((there was one(function1).call(1 -> startWith("2"))).message must contain("1 doesn't start with '2'"))
    }
    eg := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> "4.0")
    }
    eg := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> haveSize[String](3))
    }
    eg := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call(anyFunction2)
    }
    eg := {
      functionNothing.call((i:Int) => throw new Exception)
      there was one(functionNothing).call(anyFunction1)
    }
    eg := {
      functionAny.call(() => throw new Exception)
      there was one(functionAny).call(any[() => Any])
    }

    eg := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call((1, 3.0) -> "4.0")
    }
    eg := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call((1, 3.0) -> haveSize[String](3))
    }
    eg := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call(anyPartialFunction)
    }
    eg := {
      partial.call { case (i:Int, d: Double) if i > 10 => (i + d).toString }
      (there was one(partial).call((1, 3.0) -> "4.0")).message must contain("a PartialFunction defined for (1,3.0)")
    }

    eg := {
      repeated.call(1, 2, 3)
      (there was one(repeated).call(1, 2, 3)) and
      ((there was one(repeated).call(1, 2)).message must contain("WrappedArray(1, 2)"))
    }

    eg := {
      scala.concurrent.Future { Thread.sleep(200); takesSometime.call(10) }
      ((there was after(10.millis).one(takesSometime).call(10)).message must contain("Wanted but not invoked")) and
      (there was after(300.millis).one(takesSometime).call(10))
    }

    eg := {
      functionInt.call((i: Int) => i + 2)
      (there was one(functionInt).call(Map(1 -> 2))).message must contain("Argument(s) are different")
    }

    eg := {
      val foo = mock[FooComponent]
      val controller = spy(new TestController(foo))

      foo.getBar(1) returns 1
      // controller is a spy. Calling 'test' for real must not re-evaluate
      // the arguments, hence make a mock call, to register matchers
      controller.test(1)
      there were 1.times(foo).getBar(1)
    }
  }

  "stubs" - new group with list {
    eg := {
      list.add("one") returns true
      list.add("one") must_== true
    }
    eg := {
      list.add("one") returns (true, false, true)
      (list.add("one"), list.add("one"), list.add("one")) must_== ((true, false, true))
    }

    eg := {
      list.contains(anArgThat(new IsNull[String])) returns true
      list.contains(null) must_== true
    }
    eg := {
      list.contains(beMatching(".*o")) returns true
      list.contains("o") must_== true
    }
    eg := {

      trait Vet { def treat(p: Pet) = true }
      trait Pet
      case class Dog() extends Pet
      case class Cat() extends Pet
      val vet = mock[Vet]
      vet.treat(Cat())
      def isDog: Matcher[Dog] = (d: Dog) => (true, "ok", "ko")

      (there was one(vet).treat(isDog)) must not(throwA[ClassCastException])
    }
    eg := {
      list.contains(Set(1)) returns true
      list.contains(Set(1)) must_== true
      list.contains(Set(2)) must_== false
    }
    eg := {
      list.contains(List(1)) returns true
      list.contains(List(1)) must_== true
      list.contains(List(2)) must_== false
    }
    eg := {
      list.clear() throws new RuntimeException
      list.clear()
    } must throwA[RuntimeException]

    eg := {
      list.clear() throws (new RuntimeException, new IllegalArgumentException)
      tryo(list.clear())
      list.clear()
    } must throwAn[IllegalArgumentException]
    eg := {
      val mocked: java.util.List[String] = mock[java.util.List[String]].contains("o") returns true
      mocked.contains("o") must beTrue
    }
  }

  "number of calls" - new group with list {
    val list2 = mock[java.util.List[String]]

    list.add("one")
    1 to 2 foreach { i => list.add("two") }
    list2.add("one")

    eg := got { one(list).add("one") }  // equivalent to 'there was one(list).add("one")'
    eg := there were two(list).add("two")
    eg := there was atLeast(1)(list).add("two")
    eg := there was exactly(2)(list).add("two")
    eg := there were atMost(2)(list).add("two")
    eg := there was no(list).add("four")
    eg := {
      val cause = new Exception("cause")
      val e = new Exception("error", cause)
      (there was no(list).add(be_==={throw e; "four"})) must throwAn[Exception]
    }
    eg := {
      there was one(list).add("one")
      there were two(list).add("two")
      there were noMoreCallsTo(list)
    }
    eg := {
      val list3 = mock[java.util.List[String]]
      val list4 = mock[java.util.List[String]]
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
  }

  "order of calls" - new group {
    val list1 = mock[java.util.List[String]]
    val list2 = mock[java.util.List[String]]

    eg := {
      list1.get(0)
      list2.get(0)
      implicit val order = inOrder(list1, list2)
      (there was one(list1).get(0) andThen
        one(list2).get(0)).message must_== "The mock was called as expected"
    }

    eg := {
      list1.get(1) returns "1"

      // there is an out of order call but to a stubbed method
      list1.get(1)
      list1.get(0)
      list2.get(0)

      implicit val order = inOrder(ignoreStubs(list1, list2))
      (there was one(list1).get(0) andThen
        one(list2).get(0)).message must_== "The mock was called as expected"
    }

    eg := {
      list1.get(0); list1.get(1)

      implicit val order = inOrder(list1)

      val result = there was one(list1).get(1) andThen
        one(list1).get(0)

      result.message must startWith("The mock was not called as expected")
    }

    eg := {
      list1.get(0); list1.get(1)

      implicit val order = inOrder(list1)

      var result: Result = success

      new Mockito with ThrownExpectations {
        result = AsResult(there was one(list1).get(1) andThen
          one(list1).get(0))
      }

      result.message must startWith("The mock was not called as expected")
    }

    eg := {
      list1.get(0)
      list2.get(0)

      implicit val order = inOrder(list1, list2)
      (there was one(list2)(order).get(0) andThen
        one(list1)(order).get(0)).message must startWith("The mock was not called as expected")
    }

    eg := {
      list1.get(0); list1.size; list1.get(0); list1.size

      implicit val order = inOrder(list1)
      val result = there was one(list1).get(0) andThen
        one(list1).size() andThen
        no(list1) .get(0) andThen
        one(list1).size()

      result.message must startWith("The mock was not called as expected")
    }

  }

  "callbacks" - new group {
    val list = mockAs[java.util.List[String]]("list")

    eg := {
      list.get(anyInt) answers { i => "The parameter is " + i.toString}
      list.get(2) must_== "The parameter is 2"
    }

    eg := {
      list.get(anyInt) responds { case i: Int => (i + 1).toString }
      list.get(1) must_== "2"
      list.get(5) must_== "6"
    }

    eg := {
      list.set(anyInt, anyString) answers { i => "The parameters are " + (i.asInstanceOf[Array[_]].mkString("(",",",")")) }
      list.set(1,"foo") must_== "The parameters are (1,foo)"
    }

    eg := {
      list.get(anyInt) answers { (i, m) => "The parameters are " + (i.asInstanceOf[Array[_]].mkString -> m)}
      list.get(1) must_== "The parameters are (1,list)"
    }

    eg := {
      list.size answers { m => m.toString.size}
      list.size must_== 4
    }
    eg := {
      list.get(1)
      val c = capture[Int]
      there was one(list).get(c)
      c.value must_== 1
    }

    eg := {
      list.get(1)
      list.get(2)
      val c = capture[Int]
      there was two(list).get(c)
      c.values.toString === "[1, 2]"
    }
    implicit val args = main.Arguments()
  }

  "other contexts" - new group {
    eg := {
      val s = new org.specs2.mutable.Specification with Mockito {
        val list = mock[java.util.List[String]]
        "ex1" in {
          list.add("one")
          there was one(list).add("two")
          1 must_== 1 // to check if the previous expectation really fails
        }
      }
      DefaultExecutor.runSpec(s.is, Env()).filter(Fragment.isExample).map(_.executionResult.isSuccess) must contain (false)
    }

    eg := {
      val s = new org.specs2.mutable.Specification with Mockito {
        "ex1" in new specification.Scope {
          val (list1, list2) = (mock[java.util.List[String]], mock[java.util.List[String]])
          list1.add("two"); list2.add("one")
          implicit val order = inOrder(list1, list2)
          there was one(list2).add("two") andThen one(list1).add("one")
        }
      }
      DefaultExecutor.runSpec(s.is, Env()).filter(Fragment.isExample).map(_.executionResult.isSuccess) must contain (false)
    }
  }

  "mockito matchers" - new group with Mockito with ThrownExpectations {
    trait M {
      def javaList[T](a: java.util.List[T])
      def javaSet[T](a: java.util.Set[T])
      def javaCollection[T](a: java.util.Collection[T])
      def javaMap[K, V](a: java.util.Map[K, V])

      def List[T](a: List[T])
      def Set[T](a: Set[T])
      def Traversable[T](a: Traversable[T])
      def Map[K, V](a: Map[K, V])

      def varargs[T](ts: T*)
    }
    val m = mock[M]

    eg := {
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
  }

  trait list {
    val list = mock[java.util.List[String]]
    val queue = mock[scala.collection.immutable.Queue[String]]

    trait ByName {
      def call(i: =>Int) = i
      def add(i: =>Int, j: =>Int) = i + j
      def min(i: Int, j: =>Int) = i - j
      def mult(i: =>Int)(j: =>Int) = i * j
    }
    val byname = mock[ByName]

    trait WithFunction1 { def call(f: Int => String) = f(0) }
    val function1 = mock[WithFunction1]

    trait WithFunction2 { def call(f: (Int, Double) => String) = f(1, 2.0) }
    val function2 = mock[WithFunction2]

    val functionNothing = mock[WithFunctionNothing]
    val functionAny = mock[WithFunctionAny]
    val functionInt = mock[WithFunctionInt]

    trait WithPartialFunction { def call(f: PartialFunction[(Int, Double), String]) = f.apply((1, 2.0)) }
    val partial = mock[WithPartialFunction]

    trait WithRepeatedParams { def call[T](params: T*) = params.toString }
    val repeated = mock[WithRepeatedParams]

    trait TakesSometime {
      def call(i: Int) = i
    }
    val takesSometime = mock[TakesSometime]
  }
}

trait WithFunctionNothing { def call(f: Int => Nothing) = 1 }
trait WithFunctionAny { def call(f: () => Any) = 1 }
trait WithFunctionInt { def call(f: Int => Any) = 1 }

// this example comes from #428
class FooComponent {
  def getBar(id: Int): Int = id
}

class TestController(foo: FooComponent) {
  def async(f: =>Int): Int = f
  def test(id: Int) = async { foo.getBar(id) }
}

