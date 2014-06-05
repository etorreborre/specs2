package org.specs2
package execute

import control.Exceptions._
import control.Property
import reflect.ClassName._
import text.NotNullStrings._
import java.util.regex.Pattern

/**
* This trait executes a Result and returns an appropriate value when a specs2 exception is thrown
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
      case FailureException(f)                                               => f
      case SkipException(f)                                                  => f
      case PendingException(f)                                               => f
      case ErrorException(f)                                                 => f
      case DecoratedResultException(f)                                       => f
      case e: Exception                                                      => Error(e)
      case e: AssertionError if fromJUnit(e)                                 => Failure(e.getMessage.notNull, "", e.getStackTrace.toList)
      case e: java.lang.Error if simpleClassName(e) == "NotImplementedError" => Failure(e.getMessage.notNull, "", e.getStackTrace.toList)
      case e: Throwable                                                      => Error(e)
    }

  /** execute a Result and rethrow any exception or throws an exception if it is not a success */
  def effectively(result: =>Result): Result =
    try {
      result match {
        case e: Error   => throw new ErrorException(e)
        case f: Failure => throw new FailureException(f)
        case p: Pending => throw new PendingException(p)
        case s: Skipped => throw new SkipException(s)
        case r @ DecoratedResult(_, e: Error  ) => throw new DecoratedResultException(r)
        case r @ DecoratedResult(_, e: Failure) => throw new DecoratedResultException(r)
        case r @ DecoratedResult(_, e: Pending) => throw new DecoratedResultException(r)
        case r @ DecoratedResult(_, e: Skipped) => throw new DecoratedResultException(r)
        case other      => other
      }
    } catch {
      case e: FailureException                                               => throw e
      case e: SkipException                                                  => throw e
      case e: PendingException                                               => throw e
      case e: ErrorException                                                 => throw e
      case e: DecoratedResultException                                       => throw e
      case e: Exception                                                      => throw ErrorException(Error(e))
      case e: AssertionError if fromJUnit(e)                                 => throw FailureException(Failure(e.getMessage.notNull, "", e.getStackTrace.toList))
      case e: java.lang.Error if simpleClassName(e) == "NotImplementedError" => throw FailureException(Failure(e.getMessage.notNull, "", e.getStackTrace.toList))
      case e: Throwable                                                      => throw ErrorException(Error(e))
    }

  /**
   * execute a piece of code and return a result:
   *
   *  - if the code already returns a result, just keep it
   *  - if the code throws an Exception return an Error
   *  - if the code returns a value of type T, convert it to a result
   */
  def execute[T, R : AsResult](code: =>T)(convert: T => R): Result = executeEither(code)(convert) match {
    case Left(r)  => AsResult(r)
    case Right(r) => AsResult(r)
  }

  /**
   * execute a piece of code and return a result, either as a Left(failure) or a Right(value)
   */
  def executeEither[T, R](code: =>T)(implicit convert: T => R): Either[Result, R] = {
    val executed = trye(code)(identity)
    executed match {
      case Left(FailureException(f))                         => Left(f)
      case Left(SkipException(f))                            => Left(f)
      case Left(PendingException(f))                         => Left(f)
      case Left(ErrorException(f))                           => Left(f)
      case Left(DecoratedResultException(f))                 => Left(f)
      case Left(e)                                           => Left(Error(e))
      case Right(m: ResultLike)     if !m.toResult.isSuccess => Left(m.toResult)
      case Right(r: Result)         if !r.isSuccess          => Left(r)
      case Right(other)                                      => Right(convert(other))
    }
  }

  /**
   * execute a result and return either as a Left(result) if something was thrown or a Right(result)
   */
  def executeThrowable(res: =>Result): Either[Result, Result] =
    trye(res) { (e: Exception) => e match {
      case FailureException(f) => f
      case SkipException(f)    => f
      case PendingException(f) => f
      case ErrorException(f)   => f
      case other               => Error(other)
    }}

  /**
   * execute a Property returning the value if it exists and a Success result otherwise
   */
  def executeProperty[T](prop: Property[T], default: Result = Success("no value")) = executeEither(prop.optionalValue) match {
    case Right(Some(v)) => Right(v)
    case Right(None)    => Left(default)
    case Left(r)        => Left(r)
  }

  /** determine if an AssertionError has been thrown from JUnit or not */
  private def fromJUnit(e: AssertionError) = e.getStackTrace.exists((st: StackTraceElement) => JUNIT_ASSERT.matcher(st.getClassName).matches)

  private lazy val JUNIT_ASSERT = Pattern.compile(".*junit.*Assert.*")
}
object ResultExecution extends ResultExecution
