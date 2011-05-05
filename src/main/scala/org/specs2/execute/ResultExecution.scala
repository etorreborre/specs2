package org.specs2
package execute

import control.Exceptions._
import execute.Error._
import matcher.MatchResult

/**
 * This trait executes a Result and returns an approriate value when a specs2 exception is thrown
 */
trait ResultExecution { outer =>
  /** this implicit allows the execution of a Result with an `execute` method */
  implicit def resultIsExecutable(r: =>Result) = new ExecutableResult(r)
  class ExecutableResult(r: =>Result) {
    def execute = outer.execute(r)
  }
  /** execute a Result and return a Result even if there are specs2 exceptions */
  def execute(result: =>Result) =
    try {
      result
    } catch {
      case FailureException(f) => f
      case SkipException(f)    => f
      case e: Exception        => Error(e)
      case e: AssertionError   => Failure(e.getMessage, "", e.getStackTrace.toList)
      case other               => throw other
    }

  /**
   * execute a piece of code and return a result:
   *
   *  * if the code already returns a result, just keep it
   *  * if the code throws an Exception return an Error
   *  * if the code returns a value of type T, convert it to a result
   */
  def execute[T, R <% Result](code: =>T)(convert: T => R): Result = {
    val executed = trye(code)(identity)
    executed match {
      case Left(FailureException(f))                => f
      case Left(SkipException(f))                   => f
      case Left(e)                                  => Error(e)
      case Right(m: MatchResult[_]) if !m.isSuccess => m.toResult
      case Right(r: Result)         if !r.isSuccess => r
      case Right(other)                             => convert(other)
    }
  }
  
}
object ResultExecution extends ResultExecution