package org.specs2
package execute

import ResultExecution._

/**
 * Typeclass trait for anything that can be transformed to a Result
 */
trait AsResult[T] {
  def asResult(t: =>T): Result
}

object AsResult {
  /** implicit typeclass instance to create results from Booleans */
  implicit def booleanAsResult: AsResult[Boolean] = new AsResult[Boolean] {
    def asResult(t: =>Boolean): Result = Results.toResult(t)
  }

  /** implicit typeclass instance to create results from Booleans but without building location stacktraces */
  def booleanAsSimpleResult: AsResult[Boolean] = new AsResult[Boolean] {
    def asResult(b: =>Boolean): Result =
      if (b) Success("true") else Failure("false", "true", Nil)
  }

  /** typeclass instance for types which are convertible to Result */
  implicit def asResult[R](implicit convert: R => Result): AsResult[R] = new AsResult[R] {
    def asResult(r: =>R): Result = ResultExecution.execute(convert(r))
  }

  /** nicer syntax to use the AsResult syntax: AsResult(r) */
  def apply[R : AsResult](r: =>R): Result = implicitly[AsResult[R]].asResult(r)

  /** @return a Result but throw exceptions if it is not a success */
  def effectively[R : AsResult](r: =>R): Result = ResultExecution.effectively(AsResult(r))
}

/**
 * Type class to transform any value to a Result
 */
class AnyValueAsResult[T] extends AsResult[T] {
  def asResult(t: =>T) = {
    executeEither(t)(_.toString) match {
      case Left(e)  => new DecoratedResult((), e)
      case Right(v) => new DecoratedResult(v, Success())
    }
  }
}

