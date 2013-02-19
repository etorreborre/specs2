package org.specs2
package execute
import ResultExecution.execute
import org.junit.Assert

class ResultExecutionSpec extends Specification { def is =
                                                                                                                        noindent^
  "Executing a Result"                                                                                                  ^
                                                                                                                        p^
    "a failure exception must return a Failure"                                                                         ^
    { execute { throw new FailureException(Failure("failed")); success } === Failure("failed") }                        ^
    "a decorated result exception must return a DecoratedResult"                                                        ^
    { execute { throw new DecoratedResultException(DecoratedResult("", Failure("failed"))); success } ===
      DecoratedResult("", Failure("failed")) }                                                                          ^
    "an exception must return an Error"                                                                                 ^
    { execute { throw new IllegalArgumentException("exception"); success } === Error("exception") }                     ^
    "an exception must return an Error"                                                                                 ^
    { execute { throw new IllegalArgumentException("exception"); success } === Error("exception") }                     ^
    "an AssertionError must return a Failure when thrown from JUnit"                                                    ^
    { execute { Assert.fail("assertFalse"); success } === Failure("assertFalse") }                                      ^
    "an AssertionError must return a Failure, even with a null message"                                                 ^
    { execute { junit.framework.Assert.fail(null); success } === Failure("null") }                                      ^
    "an AssertionError must return an Error when not thrown from JUnit"                                                 ^
    { execute { throw new AssertionError("false"); success } === Error(new AssertionError("false")) }                   ^
    "a NotImplementedError must return a Failure"                                                                       ^
    { execute { throw NotImplementedError("???"); success } === Failure("???") }                                        ^
    "any other Throwable must return an Error"                                                                          ^
    { execute { throw new OutOfMemoryError("oome"); success } === Error(new OutOfMemoryError("oome")) }                 ^
    "any other type of Result must return itself"                                                                       ^
    { execute(success) === success }                                                                                    ^
    { execute(failure) === failure }                                                                                    ^
    { execute(skipped) === skipped }                                                                                    ^
    { execute(pending) === pending }                                                                                    ^
                                                                                                                        end


  case class NotImplementedError(msg: String) extends java.lang.Error(msg)
}
