package org.specs2
package execute

import control.Exceptions._
import execute.Error._
import matcher.MatchResult
import control.Property

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
  def execute[T, R <% Result](code: =>T)(convert: T => R): Result = executeEither(code)(convert) match {
    case Left(r)  => r
    case Right(r) => r
  }

  /**
   * execute a piece of code and return a result, either as a Left(failure) or a Right(value)
   */
  def executeEither[T, R](code: =>T)(implicit convert: T => R): Either[Result, R] = {
    val executed = trye(code)(identity)
    executed match {
      case Left(FailureException(f))                => Left(f)
      case Left(SkipException(f))                   => Left(f)
      case Left(e)                                  => Left(Error(e))
      case Right(m: MatchResult[_]) if !m.isSuccess => Left(m.toResult)
      case Right(r: Result)         if !r.isSuccess => Left(r)
      case Right(other)                             => Right(convert(other))
    }
  }

  /**
   * execute a Property returning the value if it exists and a Success result otherwise
   */
  def executeProperty[T](prop: Property[T], default: Result = Success("no value")) = executeEither(prop.optionalValue) match {
    case Right(Some(v)) => Right(v)
    case Right(None)    => Left(default)
    case Left(r)        => Left(r)
  }
}
object ResultExecution extends ResultExecution