package org.specs2
package mock
import specification._
import control.Exceptions._

class MockitoSpec extends SpecificationWithJUnit with Mockito {
  val examples = 	
"""
  Mockito is a Java library for mocking.

  The following samples are taken from the main documentation which can be found 
  here: http://mockito.googlecode.com/svn/tags/latest/javadoc/org/mockito/Mockito.html
"""^
"   When a mock is created with the mock method"^
"     it is possible to call methods to the mock" ! listMock().call1^
"     it is possible to verify that a method has been called" ! listMock().verify1^
"     if one method has not been called on a mock there will be a failure" ! listMock().verify2^
p^
"   It is also possible to return a specific value from a mocked method"^
"     then when the mocked method is called, the same values will be returned" ! listMock().return1^
"     different successive values can even be returned" ! listMock().return2^
p^
"   It is also possible to throw an exception from a mocked method"^
"     then when the mocked method is called, the exception will be thrown" ! listMock().throw1^
"     different successive exceptions can even be thrown" ! listMock().throw2^
p^
"   The number of calls to a mocked method can be checked"^
"     if the mocked method has been called once" ! calls().calls1^
"     if the mocked method has been called twice" ! calls().calls2^
"     if the mocked method has been called atLeast n times" ! calls().calls3^
"     if the mocked method has been called atMost n times" ! calls().calls4^
"     if the mocked method has never been called" ! calls().calls5^
end  

  case class listMock() {
	val list = mock[java.util.List[String]]
    def call1 = { list.add("one"); success }
	def verify1 = { 
	  list.add("one")
	  there was one(list).add("one")
	}
	def verify2 = (there was one(list).add("one")).message must startWith("The mock was not called as expected")
	def return1 = {
	  list.add("one") returns true
	  list.add("one") must_== true
	}
	def return2 = {
	  list.add("one") returns (true, false, true)
	  (list.add("one"), list.add("one"), list.add("one")) must_== (true, false, true)
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
  }
  
  case class calls() {
	val list = mock[java.util.List[String]]
	list.add("one")
	1 to 2 foreach { i => list.add("two") } 
    1 to 3 foreach { i => list.add("three") } 

    def calls1 = there was one(list).add("one")
    def calls2 = there was two(list).add("two")
    def calls3 = there was atLeast(1)(list).add("two")
    def calls4 = there was atMost(2)(list).add("two")
    def calls5 = there was no(list).add("four")
  }
//h4. Failures
//
//h4. Argument matchers
//
//{ linkTo(argumentMatchers) } allow flexible verification or stubbing.
//
//h3. Verifying the number of invocations
//
//The number of invocations (atLeast, atMost) can also be checked: { linkTo(numberOfInvocations) }
//
//h3. Verifying that invocations occur in order
//
//When calls have to happen in a given order of invocations, this can be also checked { linkTo(inOrder) }
//
//h3. Callbacks
//
//In some rare case, you want the stubbed return values to be a function of the input method parameters: { linkTo(callbacks) }
//
//h3. Annotations
//
//<ex>It is possible to use annotations to declare mocks</ex> {"""
//
//  object s5 extends Specification with Mockito {
//    // do we gain anything using Scala, compared to val mockedList = mock[List[String]]?
//    @Mock val m: List[String] = null  
//    "this needs to be inside an example because otherwise a NPE is thrown" in {
//      m.clear()
//      there was one(m).clear()
//    }
//  }
//""" snip it }
//
//{ "s5.isOk" add it }
//{ >("true") } 
//{ "s5.issues" add_> }
//
//h3. Stubbing consecutive calls (iterator-style stubbing)
//
//Sometimes we need to stub with different return value/exception for the same method call. Typical use case could be mocking iterators. Original version of Mockito did not have this feature to promote simple mocking. For example, instead of iterators one could use Iterable or simply collections. Those offer natural ways of stubbing (e.g. using real collections). 
//In rare scenarios stubbing consecutive calls could be useful, though: {"""
//
//  object s6 extends Specification with Mockito {
//    val m = mock[List[String]]
//    m.get(0) returns "hello" thenReturns "world"
//  } """ snip it }
//
//<ex>The first call returns the first value</ex>:
//
//{ "s6.m.get(0)" add it }
//{ >("hello") }
//
//<ex>The second call returns the second value</ex>:
//
//{ "s6.m.get(0)" add it }
//{ >("world") }
//
//When several values need to be stubbed this version of returns would also work: {"""
//
//  object s7 extends Specification with Mockito {
//    val m = mock[List[String]]
//    m.get(0) returns ("hello", "world")
//  }
//""" snip it }
//
//<ex>The first value is "hello"</ex>:
//{ "s7.m.get(0)" add it }
//{ >("hello") }
//
//<ex>The second value is "world"</ex>:
//{ "s7.m.get(0)" add it }
//{ >("world") }
//
//h3. Spies
//
//You can create { linkTo(spies) } of real objects. When you use a spy then the real methods are called (unless a method was stubbed).
//   
//h3. Return values
//
//Speficic { linkTo(returnValues) } can be returned on unstubbed methods.
//  
//</wiki> isSus
//
//  include(argumentMatchers)
//  include(callbacks)
//  include(inOrder)
//  include(numberOfInvocations)
//  include(spies)
//  include(returnValues)
//}
//}

}