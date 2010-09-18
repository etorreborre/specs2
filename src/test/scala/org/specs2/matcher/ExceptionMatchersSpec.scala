package org.specs2
package matcher

class ExceptionMatchersSpec extends SpecificationWithJUnit {
  val examples =
"  Exception matchers allow to check that exceptions are thrown"^
"    by specifying the expected type of exception: 'value must throwAn[Error]'"^
"      it must fail if the error is not thrown" ! e1^
"      it must succeed if the error is thrown with the expected type" ! e2^
"      it must fail if the error is thrown with the wrong type" ! e3^
p^
"    it is also possible to specify that the thrown exception is ok according to a PartialFunction"^
"      'error(boom) must throwAn[RuntimeException].like(e => e.getMessage(0) == 'b')" ! e4^
"      'error(boom) must throwAn[RuntimeException].like(e => e.getMessage(0) == 'a') will fail" ! e5^
end

  def e1 = ("hello" must throwAn[Error]).message must_==
  		    "Expected an exception of class java.lang.Error but no exception was thrown"
  def e2 = (theBlock(error("boom")) must throwA[RuntimeException]).message must_== 
	        "java.lang.RuntimeException: boom is an instance of java.lang.RuntimeException"
  def e3 = (theBlock(error("boom")) must throwAn[IllegalArgumentException]).message must_== 
	        "Expected: java.lang.IllegalArgumentException. Got: java.lang.RuntimeException: boom instead"
  def e4 = (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) == 'b' }).message must_== 
	        "java.lang.RuntimeException: boom is an instance of java.lang.RuntimeException as expected"
  def e5 = (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) == 'a' }).message must_== 
	        "Expected: java.lang.RuntimeException. Got: java.lang.RuntimeException: boom instead"

}