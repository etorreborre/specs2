package org.specs2
package execute
import ResultExecution.execute
import org.junit.Assert

class ResultExecutionSpec extends Spec { def is = s2"""

Executing a Result

 a failure exception must return a Failure
 ${ execute { throw new FailureException(Failure("failed")); success } `must` ===(Failure("failed")) }

 a decorated result exception must return a DecoratedResult
 ${ execute { throw new DecoratedResultException(DecoratedResult("", Failure("failed"))); success } `must` ===(
   DecoratedResult("", Failure("failed"))) }

 an exception must return an Error
 ${ execute { throw new IllegalArgumentException("exception"); success } `must` ===(Error("exception")) }

 an exception must return an Error
 ${ execute { throw new IllegalArgumentException("exception"); success } `must` ===(Error("exception")) }

 an AssertionError must return a Failure when thrown from JUnit
 ${ execute { Assert.fail("assertFalse"); success } `must` ===(Failure("assertFalse")) }

 an AssertionError must return a Failure, even with a null message
 ${ execute { junit.framework.Assert.fail(null); success } `must` ===(Failure("null")) }

 an AssertionError must return an Error when not thrown from JUnit
 ${ execute { throw new AssertionError("false"); success } `must` ===(Error(new AssertionError("false"))) }

 a NotImplementedError must return a Failure
 ${ execute { throw NotImplementedError("???"); success } `must` ===(Failure("???")) }

 an ExpectationError must return a Failure
 ${ execute { throw ExpectationError("???"); success } `must` ===(Failure("<expectation error>")) }

 fatal exceptions must not be caught
 ${ execute { throw new OutOfMemoryError("oome"); success } `must` throwAn[OutOfMemoryError] }

 any other type of Result must return itself
 ${ execute(success) `must` ===(success) })
 ${ execute(failure) `must` ===(failure) })
 ${ execute(skipped) `must` ===(skipped) })
 ${ execute(pending) `must` ===(pending) })
                                                                                                                        """


  case class NotImplementedError(msg: String) extends java.lang.Error(msg)
  case class ExpectationError(m: String)      extends java.lang.Error(m):
    override def toString = "<expectation error>"
}
