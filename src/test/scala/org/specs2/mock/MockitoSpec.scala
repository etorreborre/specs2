package org.specs2
package mock
import specification._
import control.Exceptions._
import org.hamcrest.core.{ IsNull }
import org.mockito.Matchers.{ anyInt }
import matcher._

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
"     a value can be returned when a parameter of the method matches" ! listMock().return3^
"       a hamcrest matcher" ! listMock().return3^
"       a specs2 matcher" ! listMock().return4^
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
"     if the mocked method has not been called after some calls" ! calls().calls6^
p^
"   Callbacks can be created to control the returned a value" ! callbacks().e1^
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
	def return3 = {
	  list.contains(argThat(new IsNull[String])) returns true
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
  }
  
  case class calls() {
	val list = mock[java.util.List[String]]
	list.add("one")
	1 to 2 foreach { i => list.add("two") } 

    def calls1 = got { one(list).add("one") }  // equivalent to 'there was one(list).add("one")'
    def calls2 = there were two(list).add("two")
    def calls3 = there was atLeast(1)(list).add("two")
    def calls4 = there were atMost(2)(list).add("two")
    def calls5 = there was no(list).add("four")
    def calls6 = {
      there was one(list).add("one")
      there were two(list).add("two")
      there were noMoreCallsTo(list)
    }
  }
  case class callbacks() {
	val list = mock[java.util.List[String]]
    def e1 = {
	  list.get(anyInt()) answers { i => "The parameter is " + i.toString }
	  list.get(2) must_== "The parameter is 2"
	}

  }
}