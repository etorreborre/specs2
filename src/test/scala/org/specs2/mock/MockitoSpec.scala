package org.specs2
package mock
import specification._
import execute.Result
import control.Exceptions._
import org.hamcrest.core.{ IsNull }
import org.mockito.Matchers.{ anyInt }
import org.mockito.stubbing._
import org.mockito.Mockito.withSettings
import org.mockito.invocation._
import matcher._
import junit.framework.AssertionFailedError

class MockitoSpec extends Specification with Mockito with ResultMatchers {  def is =
                                                                                                                        """
Mockito is a Java library for mocking.

The following samples are taken from the main documentation which can be found here:
http://mockito.googlecode.com/svn/tags/latest/javadoc/org/mockito/Mockito.html
                                                                                                                        """^p^
  "Mocks can be created"                                                                                                ^
    "with a name" 								                                                                                      ! creation().e1^
    "with a default return value"                            					                                                  ! creation().e2^
    "with a name and default return value"                            					                                        ! creation().e3^
    "with a default answer"                                           					                                        ! creation().e4^
    "with settings"                                                   					                                        ! creation().e5^
																																																												p^
  "When a mock is created with the mock method"                                                                         ^
    "it is possible to call methods on the mock" 								                                                        ! aMock().call1^
    "it is possible to verify that a method has been called" 					                                                  ! aMock().verify1^
    "if one method has not been called on a mock there will be a failure" 		                                          ! aMock().verify2^
    "it is possible to check that no calls have been made" 		                                                          ! aMock().verify3^
    "it is possible to pass byname parameters"          		                                                            ! aMock().verify4^
    "it is possible to check byname parameters"          		                                                            ! aMock().verify5^
    "it is possible to check a function parameter"          		                                                        ^
      "with one argument"                                                                                               ! aMock().verify6^
      "with one argument and a matcher for the return value"                                                            ! aMock().verify7^
      "with n arguments"                                                                                                ! aMock().verify8^
      "with n arguments and a matcher for the return value"                                                             ! aMock().verify9^
      "as being anything"                                                                                               ! aMock().verify10^
                                                                                                                          endp^
  "It is also possible to return a specific value from a mocked method"                                                 ^
    "then when the mocked method is called, the same values will be returned" 	                                        ! aMock().return1^
    "different successive values can even be returned" 						                                                      ! aMock().return2^
    "a value can be returned when a parameter of the method matches" 			                                              ^
      "a hamcrest matcher" 													                                                                    ! aMock().return3^
      "a specs2 matcher" 														                                                                    ! aMock().return4^
                                                                                                                        endp^
  "It is also possible to throw an exception from a mocked method"                                                      ^
    "then when the mocked method is called, the exception will be thrown" 		                                          ! aMock().throw1^
    "different successive exceptions can even be thrown" 						                                                    ! aMock().throw2^
                                                                                                                        p^
  "A mock can be created and stubbed at the same time"                        	                                        ! aMock().mockAndStub^
                                                                                                                        p^
  "The number of calls to a mocked method can be checked"                                                               ^
    "if the mocked method has been called once"                                                                         ! calls().calls1^
    "if the mocked method has been called twice"                                                                        ! calls().calls2^
    "if the mocked method has been called atLeast n times"                                                              ! calls().calls3^
    "if the mocked method has been called atMost n times"                                                               ! calls().calls4^
    "if the mocked method has never been called"                                                                        ! calls().calls5^
    "if the verification throws an exception, it will be reported as an Error"                                          ! calls().calls6^
    "if the mocked method has not been called after some calls"                                                         ! calls().calls7^
    "if the mocked method has not been called after some calls - ignoring stubs"                                        ! calls().calls8^
                                                                                                                        p^
  "The order of calls to a mocked method can be checked"                                                                ^
    "with 2 calls that were indeed in order"                                                                            ! ordered().asExpected1^
    "with 2 calls that were indeed in order - ignoring stubbed methods"                                                 ! ordered().asExpected2^
    "with 2 calls that were indeed not in order"                                                                        ! ordered().failed^
    "with 3 calls that were indeed not in order"                                                                        ! ordered().failed2^
                                                                                                                        p^
  "Callbacks can be created to control the returned a value"                                                            ! callbacks().c1^
                                                                                                                        p^
  "A parameter can be captured in order to check its value"                                                             ! captured().e1^
  "A parameter can be captured in order to check its successive values"                                                 ! captured().e2^
                                                                                                                        p^
  "The Mockito trait is reusable in other contexts"                                                                     ^
    "in mutable specs"                                                                                                  ! reuse().e1^
    "in JUnit"                                                                                                          ! reuse().e2^
                                                                                                                        end
    
  case class creation() {
    def e1 = { 
			val list = mock[java.util.List[String]].as("list1")
			(there was one(list).add("one")).message must contain("list1.add(\"one\")")
		}
    def e2 = {
			val list = mock[java.util.List[String]].settings(defaultReturn = 10)
      list.size must_== 10
    }
    def e3 = {
			val list = mock[java.util.List[String]].settings(name = "list1", defaultReturn = 10, extraInterfaces = classesOf[Cloneable, Serializable])
      (list.size must_== 10) and 
			((there was one(list).add("one")).message must contain("list1.add(\"one\")"))
    }
    def e4 = {
			val list = mock[java.util.List[String]].defaultAnswer((p1: InvocationOnMock) => "hello")
      list.get(0) must_== "hello" 
    }
    def e5 = {
			val list = mock[java.util.List[String]](withSettings.name("list1"))
			(there was one(list).add("one")).message must contain("list1.add(\"one\")")
		}
	}
	
	case class aMock() {
    val list = mock[java.util.List[String]]
    val queue = mock[scala.collection.immutable.Queue[String]]

    trait ByName { def call(i: =>Int) = i }
    val byname = mock[ByName]

    trait WithFunction1 { def call(f: Int => String) = f(0) }
    val function1 = mock[WithFunction1]

    trait WithFunction2 { def call(f: Function2[Int, Double, String]) = f(1, 2.0) }
    val function2 = mock[WithFunction2]

    def call1 = { list.add("one"); success }

    def verify1 = {
      list.add("one")
      there was one(list).add("one")
    }
    def verify2 = (there was one(list).add("one")).message must startWith("The mock was not called as expected")

    def verify3 = there were noCallsTo(list)

    def verify4 = {
      byname.call(10)
      there was one(byname).call(10)
    }
    def verify5 = {
      byname.call(10)
      there was one(byname).call(be_>(5))
    }

    def verify6 = {
      function1.call((_:Int).toString)
      there was one(function1).call(1 -> "1")
    }
    def verify7 = {
      function1.call((_:Int).toString)
      there was one(function1).call(1 -> startWith("1"))
    }
    def verify8 = {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> "4.0")
    }
    def verify9 = {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call((1, 3.0) -> haveSize[String](3))
    }
    def verify10 = {
      function2.call((i:Int, d: Double) => (i + d).toString)
      there was one(function2).call(anyFunction2)
    }

    def return1 = {
      list.add("one") returns true
      list.add("one") must_== true
    }
    def return2 = {
      list.add("one") returns (true, false, true)
      (list.add("one"), list.add("one"), list.add("one")) must_== (true, false, true)
    }
    def return3 = {
      list.contains(anArgThat(new IsNull[String])) returns true
      list.contains(null) must_== true
    }
    def return4 = {
      list.contains(argThat(beMatching(".*o"))) returns true
      list.contains("o") must_== true
    }
    def throw1 = {
      list.clear() throws new RuntimeException
      list.clear()
    } must throwA[RuntimeException]

    def throw2 = {
      list.clear() throws (new RuntimeException, new IllegalArgumentException)
      tryo(list.clear())
      list.clear()
    } must throwAn[IllegalArgumentException]

    def mockAndStub = {
      val mocked: java.util.List[String] = mock[java.util.List[String]].contains("o") returns true
      mocked.contains("o") must beTrue
    }

  }
  
  case class calls() {
    val list = mock[java.util.List[String]]
    val list2 = mock[java.util.List[String]]

    list.add("one")
    1 to 2 foreach { i => list.add("two") }
    list2.add("one")

    def calls1 = got { one(list).add("one") }  // equivalent to 'there was one(list).add("one")'
    def calls2 = there were two(list).add("two")
    def calls3 = there was atLeast(1)(list).add("two")
    def calls4 = there were atMost(2)(list).add("two")
    def calls5 = there was no(list).add("four")
    def calls6 = {
      val cause = new Exception("cause")
      val e = new Exception("error", cause)
      (there was no(list).add(be_==={throw e; "four"})) must throwAn[Exception]
    }
    def calls7 = {
      there was one(list).add("one")
      there were two(list).add("two")
      there were noMoreCallsTo(list)
    }
    def calls8 = {
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
  case class callbacks() {
    val list = mock[java.util.List[String]]
    def c1 = {
      list.get(anyInt) answers { i => "The parameter is " + i.toString }
      list.get(2) must_== "The parameter is 2"
    }
  }
  case class ordered() {
    val list1 = mock[java.util.List[String]]
    val list2 = mock[java.util.List[String]]

    def asExpected1: Result = {
      list1.get(0)
      list2.get(0)
      implicit val order = inOrder(list1, list2)
      (there was one(list1).get(0) then
                 one(list2).get(0)).message must_== "The mock was called as expected"
    }

    def asExpected2: Result = {
      list1.get(1) returns "1"

      // there is an out of order call but to a stubbed method
      list1.get(1)
      list1.get(0)
      list2.get(0)

      implicit val order = inOrder(ignoreStubs(list1, list2))
      (there was one(list1).get(0) then
        one(list2).get(0)).message must_== "The mock was called as expected"
    }

    def failed = {
      list1.get(0)
      list2.get(0)
      implicit val order = inOrder(list1, list2)
      (there was one(list2)(order).get(0) then
                 one(list1)(order).get(0)).message must startWith("The mock was not called as expected")
    }

    def failed2: execute.Result = {
      list1.get(0); list1.size; list1.get(0); list1.size;

      implicit val order = inOrder(list1)
      val result = there was one(list1).get(0) then
                             one(list1).size() then
                             no(list1) .get(0) then
                             one(list1).size()

      result.message must startWith("The mock was not called as expected")
    }
  }
  case class captured() {
    val list = mock[java.util.List[String]]
    def e1 = {
     list.get(1)
      val c = capture[Int]
       there was one(list).get(c)
      c.value must_== 1
    }

    def e2 = {
      list.get(1)
      list.get(2)
      val c = capture[Int]
      there was two(list).get(c)
      c.values.toString must_== "[1, 2]"
    }
  }

}

case class reuse() extends FragmentExecution with MustMatchers {
  implicit val args = main.Arguments()

  def e1 = {
    val s = new mutable.Specification with Mockito {
      val list = mock[java.util.List[String]]
      "ex1" in {
        list.add("one")
        there was one(list).add("two")
        1 must_== 1 // to check if the previous expectation really fails
      }
    }
    s.content.fragments.collect { case Example(d, r) => executeBody(r()).isSuccess } must contain (false)
  }

  def e2 = {
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