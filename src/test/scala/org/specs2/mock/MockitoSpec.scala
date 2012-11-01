package org.specs2
package mock

import specification.{Tags, Groups, FragmentExecution, Example}
import control.Exceptions._
import org.hamcrest.core.IsNull
import org.mockito.Mockito.withSettings
import org.mockito.invocation._
import matcher._
import junit.framework.AssertionFailedError

class MockitoSpec extends Specification with Mockito with ResultMatchers with Groups {  def is =
                                                                                                                        """
Mockito is a Java library for mocking.

The following samples are taken from the main documentation which can be found here:
http://mockito.googlecode.com/svn/tags/latest/javadoc/org/mockito/Mockito.html
                                                                                                                        """^p^
  "CREATION"                                                                                                            ^
  "Mocks can be created"                                                                                                ^
    "with a name" 								                                                                                      ! g1().e1^
    "with a default return value"                            					                                                  ! g1().e2^
    "with a name and default return value"                            					                                        ! g1().e3^
    "with a default answer"                                           					                                        ! g1().e4^
    "with settings"                                                   					                                        ! g1().e5^
																																																												p^
  "VERIFICATION"                                                                                                        ^
  "When a mock is created with the mock method"                                                                         ^
    "it is possible to call methods on the mock" 								                                                        ! g2().e1^
    "it is possible to verify that a method has been called" 					                                                  ! g2().e2^
    "if one method has not been called on a mock there will be a failure" 		                                          ! g2().e3^
    "it is possible to check that no calls have been made" 		                                                          ! g2().e4^
    "it is possible to pass byname parameters"          		                                                            ! g2().e5^
    "it is possible to check byname parameters"          		                                                            ! g2().e7^
    "it is possible to check a function parameter"          		                                                        ^
      "with one argument"                                                                                               ! g2().e7^
      "with one argument and a matcher for the return value"                                                            ! g2().e8^
      "with n arguments"                                                                                                ! g2().e9^
      "with n arguments and a matcher for the return value"                                                             ! g2().e10^
      "as being anything"                                                                                               ! g2().e11^
      "with Nothing as the return type"                                                                                 ! g2().e12^
      "with Any as the return type"                                                                                     ! g2().e13^
                                                                                                                        p^
    "it is possible to check a partial function parameter"          		                                                ^
      "with n arguments"                                                                                                ! g2().e14^
      "with n arguments and a matcher for the return value"                                                             ! g2().e15^
      "as being anything"                                                                                               ! g2().e16^
      "when the argument is not defined"                                                                                ! g2().e17^
                                                                                                                        p^
    "it is possible to verify a function with implicit conversions"          		                                        ^
      "with a single converted parameter"                                                                               ! g2().e18^
      "with a single converted parameter, using a matcher"                                                              ! g2().e19^
                                                                                                                        p^
    "it is possible to verify a function with repeated parameters"          		                                        ! g2().e20^
                                                                                                                        endp^
  "It is also possible to return a specific value from a mocked method"                                                 ^
    "then when the mocked method is called, the same values will be returned" 	                                        ! g3().e1^
    "different successive values can even be returned" 						                                                      ! g3().e2^
    "a value can be returned when a parameter of the method matches" 			                                              ^
      "a hamcrest matcher" 													                                                                    ! g3().e3^
      "a specs2 matcher" 														                                                                    ! g3().e4^
      "a set"           														                                                                    ! g3().e5^
                                                                                                                          endp^
  "It is also possible to throw an exception from a mocked method"                                                      ^
    "then when the mocked method is called, the exception will be thrown" 		                                          ! g4().e1^
    "different successive exceptions can even be thrown" 						                                                    ! g4().e2^
                                                                                                                        p^
  "A mock can be created and stubbed at the same time"                        	                                        ! g5().e1^
                                                                                                                        p^
  "NUMBER OF CALLS"                                                                                                     ^
  "The number of calls to a mocked method can be checked"                                                               ^
    "if the mocked method has been called once"                                                                         ! g6().e1^
    "if the mocked method has been called twice"                                                                        ! g6().e2^
    "if the mocked method has been called atLeast n times"                                                              ! g6().e3^
    "if the mocked method has been called atMost n times"                                                               ! g6().e4^
    "if the mocked method has never been called"                                                                        ! g6().e5^
    "if the verification throws an exception, it will be reported as an Error"                                          ! g6().e6^
    "if the mocked method has not been called after some calls"                                                         ! g6().e7^
    "if the mocked method has not been called after some calls - ignoring stubs"                                        ! g6().e8^
                                                                                                                        p^
  "ORDER OF CALLS"                                                                                                      ^
  "The order of calls to a mocked method can be checked"                                                                ^
    "with 2 calls that were indeed in order"                                                                            ! g7().e1^
    "with 2 calls that were indeed in order - ignoring stubbed methods"                                                 ! g7().e2^
    "with 2 calls that were indeed not in order"                                                                        ! g7().e3^
    "with 3 calls that were indeed not in order"                                                                        ! g7().e4^
                                                                                                                        p^
  "ANSWERS & PARAMETERS CAPTURE"                                                                                        ^
  "Answers can be created to control the returned a value"                                                              ! g8().e1^
  "Answers can use the mock instance as the second parameter"                                                           ! g8().e2^
  "Answers can use the mock instance, even when the method has 0 parameters"                                            ! g8().e3^
                                                                                                                        p^
  "A parameter can be captured in order to check its value"                                                             ! g9().e1^
  "A parameter can be captured in order to check its successive values"                                                 ! g9().e2^
                                                                                                                        p^
  "The Mockito trait is reusable in other contexts"                                                                     ^
    "in mutable specs"                                                                                                  ! g10().e1^
    "in JUnit"                                                                                                          ! g10().e2^
                                                                                                                        end
    
  "creation" - new g1 {
    e1 := {
			val list = mock[java.util.List[String]].as("list1")
			(there was one(list).add("one")).message must contain("list1.add(\"one\")")
		}
    e2 := {
			val list = mock[java.util.List[String]].settings(defaultReturn = 10)
      list.size must_== 10
    }
    e3 := {
			val list = mock[java.util.List[String]].settings(name = "list1", defaultReturn = 10, extraInterfaces = classesOf[Cloneable, Serializable])
      (list.size must_== 10) and 
			((there was one(list).add("one")).message must contain("list1.add(\"one\")"))
    }
    e4 := {
			val list = mock[java.util.List[String]].defaultAnswer((p1: InvocationOnMock) => "hello")
      list.get(0) must_== "hello" 
    }
    e5 := {
			val list = mock[java.util.List[String]](withSettings.name("list1"))
			(there was one(list).add("one")).message must contain("list1.add(\"one\")")
		}
	}
	"verification" - new g2 with list {
    e1 := { list.add("one"); success }
    e2 := {
      list.add("one")
      there was one(list).add("one")
    }
    e3 := (there was one(list).add("one")).message must startWith("The mock was not called as expected")

    e4 := there were noCallsTo(list)

    e5 := {
      byname.call(10)
      there was one(byname).call(10)
    }
    e6 := {
      byname.call(10)
      there was one(byname).call(be_>(5))
    }
    e7 := {
      function1.call((_:Int).toString)
      there was one(function1).call(1 -> "1")
    }
    e8 := {
      function1.call((_:Int).toString)
      (there was one(function1).call(1 -> startWith("1"))) and
      ((there was one(function1).call(1 -> startWith("2"))) returns "'1' doesn't start with '2'")
    }
    e9 := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> "4.0")
    }
    e10 := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> haveSize[String](3))
    }
    e11 := {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call(anyFunction2)
    }
    e12 := {
      functionNothing.call((i:Int) => throw new Exception)
      there was one(functionNothing).call(anyFunction1)
    }
    e13 := {
      functionAny.call(() => throw new Exception)
      there was one(functionAny).call(any[() => Any])
    }
    e14 := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call((1, 3.0) -> "4.0")
    }
    e15 := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call((1, 3.0) -> haveSize[String](3))
    }
    e16 := {
      partial.call { case (i:Int, d: Double) => (i + d).toString }
      there was one(partial).call(anyPartialFunction)
    }
    e17 := {
      partial.call { case (i:Int, d: Double) if i > 10 => (i + d).toString }
      there was one(partial).call((1, 3.0) -> "4.0") returns "a PartialFunction defined for (1,3.0)"
    }
    e18 := {
      converted.call("test")
      there was one(converted).call("test")
    }
    e19 := {
      converted.call("test")
      there was one(converted).call(startWith("t"))
    }
    e20 := {
      repeated.call(1, 2, 3)
      (there was one(repeated).call(1, 2, 3)) and
      ((there was one(repeated).call(1, 2)) returns "WrappedArray(1, 2)")
    }
  }
  "stubs" - new g3 with list {
    e1 := {
      list.add("one") returns true
      list.add("one") must_== true
    }
    e2 := {
      list.add("one") returns (true, false, true)
      (list.add("one"), list.add("one"), list.add("one")) must_== (true, false, true)
    }
    e3 := {
      list.contains(anArgThat(new IsNull[String])) returns true
      list.contains(null) must_== true
    }
    e4 := {
      list.contains(argThat(beMatching(".*o"))) returns true
      list.contains("o") must_== true
    }
    e5 := {
      list.contains(Set(1)) returns true
      list.contains(Set(1)) must_== true
      list.contains(Set(2)) must_== false
    }
  }
  "thrown exceptions" - new g4 with list {
    e1 := {
      list.clear() throws new RuntimeException
      list.clear()
    } must throwA[RuntimeException]

    e2 := {
      list.clear() throws (new RuntimeException, new IllegalArgumentException)
      tryo(list.clear())
      list.clear()
    } must throwAn[IllegalArgumentException]
  }
  "mockAndStub" - new g5 {
    e1 := {
      val mocked: java.util.List[String] = mock[java.util.List[String]].contains("o") returns true
      mocked.contains("o") must beTrue
    }
  }
  "number of calls" - new g6 with list {
    val list2 = mock[java.util.List[String]]

    list.add("one")
    1 to 2 foreach { i => list.add("two") }
    list2.add("one")

    e1 := got { one(list).add("one") }  // equivalent to 'there was one(list).add("one")'
    e2 := there were two(list).add("two")
    e3 := there was atLeast(1)(list).add("two")
    e4 := there were atMost(2)(list).add("two")
    e5 := there was no(list).add("four")
    e6 := {
      val cause = new Exception("cause")
      val e = new Exception("error", cause)
      (there was no(list).add(be_==={throw e; "four"})) must throwAn[Exception]
    }
    e7 := {
      there was one(list).add("one")
      there were two(list).add("two")
      there were noMoreCallsTo(list)
    }
    e8 := {
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
  "order of calls" - new g7 {
    val list1 = mock[java.util.List[String]]
    val list2 = mock[java.util.List[String]]

    e1 := {
      list1.get(0)
      list2.get(0)
      implicit val order = inOrder(list1, list2)
      (there was one(list1).get(0) then
        one(list2).get(0)).message must_== "The mock was called as expected"
    }

    e2 := {
      list1.get(1) returns "1"

      // there is an out of order call but to a stubbed method
      list1.get(1)
      list1.get(0)
      list2.get(0)

      implicit val order = inOrder(ignoreStubs(list1, list2))
      (there was one(list1).get(0) then
        one(list2).get(0)).message must_== "The mock was called as expected"
    }

    e3 := {
      list1.get(0)
      list2.get(0)
      implicit val order = inOrder(list1, list2)
      (there was one(list2)(order).get(0) then
        one(list1)(order).get(0)).message must startWith("The mock was not called as expected")
    }

    e4 := {
      list1.get(0); list1.size; list1.get(0); list1.size;

      implicit val order = inOrder(list1)
      val result = there was one(list1).get(0) then
        one(list1).size() then
        no(list1) .get(0) then
        one(list1).size()

      result.message must startWith("The mock was not called as expected")
    }
  }
  "callbacks" - new g8 {
    val list = mockAs[java.util.List[String]]("list")

    e1 := {
      list.get(anyInt) answers { i => "The parameter is " + i.toString }
      list.get(2) must_== "The parameter is 2"
    }
    e2 := {
      list.get(anyInt) answers { (i, m) => "The parameters are " + (i.asInstanceOf[Array[_]].mkString, m) }
      list.get(1) must_== "The parameters are (1,list)"
    }
    e3 := {
      list.size answers { m => m.toString.size }
      list.size must_== 4
    }
  }
  "arguments capture" - new g9 with list {
    e1 := {
      list.get(1)
      val c = capture[Int]
      there was one(list).get(c)
      c.value must_== 1
    }

    e2 := {
      list.get(1)
      list.get(2)
      val c = capture[Int]
      there was two(list).get(c)
      c.values.toString === "[1, 2]"
    }
  }
  "reuse" - new g10 {
    implicit val args = main.Arguments()

    e1 := {
      val s = new mutable.Specification with Mockito {
        val list = mock[java.util.List[String]]
        "ex1" in {
          list.add("one")
          there was one(list).add("two")
          1 must_== 1 // to check if the previous expectation really fails
        }
      }
      s.content.fragments.collect { case Example(d, r) => FragmentExecution.executeBody(r()).isSuccess } must contain (false)
    }

    e2 := {
      val s = new Mockito with JUnitExpectations {
        val list = mock[java.util.List[String]]
        def test = {
          list.add("one")
          there was one(list).add("two")
          1 must_== 1 // to check if the previous expectation really fails
        }
      }
      s.test must throwAn[AssertionFailedError]
    }
  }

  trait list {
    val list = mock[java.util.List[String]]
    val queue = mock[scala.collection.immutable.Queue[String]]

    trait ByName { def call(i: =>Int) = i }
    val byname = mock[ByName]

    trait WithFunction1 { def call(f: Int => String) = f(0) }
    val function1 = mock[WithFunction1]

    trait WithFunction2 { def call(f: (Int, Double) => String) = f(1, 2.0) }
    val function2 = mock[WithFunction2]

    val functionNothing = mock[WithFunctionNothing]
    val functionAny = mock[WithFunctionAny]

    trait WithPartialFunction { def call(f: PartialFunction[(Int, Double), String]) = f.apply((1, 2.0)) }
    val partial = mock[WithPartialFunction]

    case class WrappedString(s: String)
    implicit def wrap(s: String): WrappedString = WrappedString(s)
    trait WithImplicitConversion { def call[T <% WrappedString](s: T) = s.toString }
    val converted = mock[WithImplicitConversion]

    trait WithRepeatedParams { def call[T](params: T*) = params.toString }
    val repeated = mock[WithRepeatedParams]
  }
}

trait WithFunctionNothing { def call(f: Int => Nothing) = 1 }
trait WithFunctionAny { def call(f: () => Any) = 1 }

