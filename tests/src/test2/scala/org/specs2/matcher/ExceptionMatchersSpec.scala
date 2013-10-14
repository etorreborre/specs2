package org.specs2
package matcher

import sys._
import execute.ResultExecution
import specification._
 
class ExceptionMatchersSpec extends script.Specification with ResultMatchers with Groups { def is = s2"""

 Exception matchers allow to check that exceptions are thrown

 By type
 =======

 by specifying the expected type of exception: 'value must throwAn[Exception]'
   + it must fail if the exception is not thrown
   + it must succeed if the exception is thrown with the expected type
   + it must fail if the exception is thrown with the wrong type
   + it must not fail if the exception is not thrown and the matcher is negated
   + it must not fail when the exception type is not specified
   + it must return an Error when an Exception is expected and a java.lang.Error is thrown

 With a PartialFunction
 ======================

 it is also possible to specify that the thrown exception is ok according to a PartialFunction
   + 'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'b')
   + 'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'a') will fail

 With a regular expression
 =========================

 more simply the exception message can be specified with a regular expression
   + 'error(boom) must throwA[RuntimeException](message = 'boo')
   + for a multi-line string

 With a specific exception
 =========================

 by specifying the expected exception: 'value must throwA(new java.lang.RuntimeException('wrong')'
   + it must fail if the exception is not thrown at all
   + it must succeed if an exception of same class and message is thrown
   + it must fail if an exception of a different class and same message is thrown
   + it must fail if an exception of a same class and different message is thrown
   it can be refined with a 'like' expression
     + failing if the catched expression doesn't satisfy the partial function
     + succeeding otherwise

 With combinators
 ================

 + negating a throw matcher must return the proper success message
                                                                                                              """

  "exception types" - new group {
    eg := ("hello" must throwAn[Error]).message must_== "Expected: java.lang.Error. Got nothing"

    eg := (theBlock(error("boom")) must throwA[RuntimeException]).message must_==
      "Got the exception java.lang.RuntimeException: boom"

    eg := (theBlock(error("boom")) must throwAn[IllegalArgumentException]).message must_==
      "Expected: java.lang.IllegalArgumentException. Got: java.lang.RuntimeException: boom instead"

    eg := (1 must not throwA(new Exception)).toResult must beSuccessful

    eg := ({sys.error("boom"); 1} must not throwA).toResult must beFailing

    eg := {
      ResultExecution.execute(({throw new StackOverflowError("play again"); 1} must not(throwAn[Exception])).toResult) must beError
    }

  }

  "Partial function" - new group {
    eg := (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) === 'b' }).message must_==
      "Got the exception java.lang.RuntimeException: boom ('b' is equal to 'b')"

    eg := (theBlock(error("boom")) must throwA[RuntimeException].like { case e => e.getMessage()(0) === 'a' }).message must_==
      "Expected: java.lang.RuntimeException. Got: java.lang.RuntimeException: boom instead ('b' is not equal to 'a')"
  }

  "regular expression" - new group {
    eg := (theBlock(error("boom")) must throwA[RuntimeException](message = "boo")).message must_==
      "Got the exception java.lang.RuntimeException: boom ('boom' matches '\\s*.*\\s*boo\\s*.*\\s*')"

    eg := (theBlock(error("boom\nbang\nbong")) must throwA[RuntimeException](message = "bang")).message must_==
      "Got the exception java.lang.RuntimeException: boom\nbang\nbong ('boom\nbang\nbong' matches '\\s*.*\\s*bang\\s*.*\\s*')"
  }

  "specific exception" - new group {
    eg := ("hello" must throwA(new RuntimeException("boom"))).message must_==
      "Expected: java.lang.RuntimeException: boom. Got nothing"

    eg := (theBlock(error("boom")) must throwAn(new RuntimeException("boom"))).message must_==
      "Got the exception java.lang.RuntimeException: boom"

    eg := (theBlock(error("boom")) must throwAn(new IllegalArgumentException("boom"))).message must_==
      "Expected: java.lang.IllegalArgumentException: boom. Got: java.lang.RuntimeException: boom instead"

    eg := (theBlock(error("boom")) must throwAn(new RuntimeException("bang"))).message must_==
      "Expected: java.lang.RuntimeException: bang. Got: java.lang.RuntimeException: boom instead"

    case class UserError(name: String, message: String) extends RuntimeException(message)
    eg := (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).
      like { case UserError(name, _) => name must endWith("2") }).message must endWith("('me' doesn't end with '2')")

    eg := (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).
      like { case UserError(name, _) => name must startWith("m") }).message must beMatching("Got the exception .*")
  }

  "combinators" - new group {
    eg := (1 must not(throwAn[Exception])).toResult.message must_== "Expected: java.lang.Exception. Got nothing"
  }


}