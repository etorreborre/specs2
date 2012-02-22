package org.specs2
package execute
import ResultExecution.execute

class ResultExecutionSpec extends Specification { def is =
                                                                                                      noindent^
  "Executing a Result"                                                                                ^
                                                                                                      p^
    "a failure exception must return a Failure"                                                       ^
    { execute { throw new FailureException(Failure("failed")); success } === Failure("failed") }      ^
    "an exception must return an Error"                                                               ^
    { execute { throw new IllegalArgumentException("exception"); success } === Error("exception") }   ^
    "an exception must return an Error"                                                               ^
    { execute { throw new IllegalArgumentException("exception"); success } === Error("exception") }   ^
    "an AssertionError must return a Failure"                                                         ^
    { execute { throw new AssertionError("assertFalse"); success } === Failure("assertFalse") }       ^
    "a NotImplementedError must return a Failure"                                                     ^
    { execute { throw NotImplementedError("???"); success } === Failure("???") }                      ^
    "any other type of Result must return itself"                                                     ^
    { execute(success) === success }                                                                  ^
    { execute(failure) === failure }                                                                  ^
    { execute(skipped) === skipped }                                                                  ^
    { execute(pending) === pending }                                                                  ^
                                                                                                      end


  case class NotImplementedError(msg: String) extends java.lang.Error(msg)
}
