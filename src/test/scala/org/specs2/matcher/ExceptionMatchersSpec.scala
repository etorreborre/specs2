package org.specs2
package matcher

import sys._
import execute.ResultExecution
 
class ExceptionMatchersSpec extends Specification with ResultMatchers { def is = s2"""

 Exception matchers allow to check that exceptions are thrown
   by specifying the expected type of exception: 'value must throwAn[Exception]'
     it must fail if the exception is not thrown                                                               $e1
     it must succeed if the exception is thrown with the expected type                                         $e2
     it must fail if the exception is thrown with the wrong type                                               $e3
     it must not fail if the exception is not thrown and the matcher is negated                               $e3_1
     it must not fail when the exception type is not specified                                                 $e3_2
     it must return an Error when an Exception is expected and a java.lang.Error is thrown                     $e3_3

   it is also possible to specify that the thrown exception is ok according to a PartialFunction
     'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'b')                            $e4
     'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'a') will fail                   $e5

   more simply the exception message can be specified with a regular expression
     'error(boom) must throwA[RuntimeException](message = 'boo')                                              $e4_1

   by specifying the expected exception: 'value must throwA(new java.lang.RuntimeException('wrong')'
     it must fail if the exception is not thrown at all                                                       $e6
     it must succeed if an exception of same class and message is thrown                                       $e7
     it must fail if an exception of a different class and same message is thrown                             $e8
     it must fail if an exception of a same class and different message is thrown                             $e9
     it can be refined with a 'like' expression
       failing if the catched expression doesn't satisfy the partial function                                 $e10
       succeeding otherwise                                                                                   $e11

   negating a throw matcher must return the proper success message                                            $e12
                                                                                                                        """

  def e1 = ("hello" must throwAn[Error]).message must_== "Expected: java.lang.Error. Got nothing"

  def e2 = (theBlock(error("boom")) must throwA[RuntimeException]).message must_==
          "Got the exception java.lang.RuntimeException: boom"

  def e3 = (theBlock(error("boom")) must throwAn[IllegalArgumentException]).message must_== 
          "Expected: java.lang.IllegalArgumentException. Got: java.lang.RuntimeException: boom instead"

  def e3_1 = (1 must not throwA(new Exception)).toResult must beSuccessful

  def e3_2 = ({sys.error("boom"); 1} must not throwA).toResult must beFailing

  def e3_3 = {
    ResultExecution.execute(({throw new StackOverflowError("play again"); 1} must not(throwAn[Exception])).toResult) must beError
  }

  def e4 = (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) === 'b' }).message must_==
          "Got the exception java.lang.RuntimeException: boom ('b' is equal to 'b')"

  def e4_1 = (theBlock(error("boom")) must throwA[RuntimeException](message = "boo")).message must_==
          "Got the exception java.lang.RuntimeException: boom ('boom' matches '.*boo.*')"

  def e5 = (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) === 'a' }).message must_==
          "Expected: java.lang.RuntimeException. Got: java.lang.RuntimeException: boom instead ('b' is not equal to 'a')"

  def e6 = ("hello" must throwA(new RuntimeException("boom"))).message must_==
          "Expected: java.lang.RuntimeException: boom. Got nothing"

  def e7 = (theBlock(error("boom")) must throwAn(new RuntimeException("boom"))).message must_== 
        "Got the exception java.lang.RuntimeException: boom"

  def e8 = (theBlock(error("boom")) must throwAn(new IllegalArgumentException("boom"))).message must_== 
        "Expected: java.lang.IllegalArgumentException: boom. Got: java.lang.RuntimeException: boom instead"

  def e9 = (theBlock(error("boom")) must throwAn(new RuntimeException("bang"))).message must_== 
        "Expected: java.lang.RuntimeException: bang. Got: java.lang.RuntimeException: boom instead"

  case class UserError(name: String, message: String) extends RuntimeException(message)
  def e10 = (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).
        like { case UserError(name, _) => name must endWith("2") }).message must endWith("('me' doesn't end with '2')")

  def e11 = (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).
         like { case UserError(name, _) => name must startWith("m") }).message must beMatching("Got the exception .*")

  def e12 = (1 must not(throwAn[Exception])).toResult.message must_== "Expected: java.lang.Exception. Got nothing"
}