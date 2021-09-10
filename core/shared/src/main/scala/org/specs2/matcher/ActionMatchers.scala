package org.specs2
package matcher

import control.*
import execute.*
import text.Regexes.*
import concurrent.*
import scala.concurrent.*

/** Matchers for Action values
  */
trait ActionMatchers extends ValueChecks:

  given executionEnv: ExecutionEnv =
    ExecutionEnv.fromGlobalExecutionContext

  given ExecutionContext =
    executionEnv.executionContext

  def beOk[T]: FutureMatcher[Action[T]] =
    beOk[T, T](identity)

  def beOk[T, R](f: T => R): FutureMatcher[Action[T]] =
    FutureMatcher((_: Action[T]).map(f).runFuture(executionEnv))

  def beOk[T](check: ValueCheck[T]): FutureMatcher[Action[T]] =
    FutureMatcher((_: Action[T]).map(check.check).runFuture(executionEnv))

  def beOkWithValue[T](t: T): FutureMatcher[Action[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: FutureMatcher[Action[T]] =
    FutureMatcher { (action: Action[T]) =>
      action.runFuture(executionEnv).map(_ => Failure("a failure was expected")).recover(_ => Success("ok"))
    }

  def beKo[T](message: String): FutureMatcher[Action[T]] =
    FutureMatcher { (action: Action[T]) =>
      action.runFuture(executionEnv).map(_ => Failure(s"a failure with message $message was expected")).recover {
        case t if t.getMessage `matchesSafely` message => Success("ok")
        case t => Failure(s"the action failed with message ${t.getMessage}. Expected: $message"),
      }
    }

  extension [T](action: Action[T])
    infix def must(m: FutureMatcher[Action[T]]): Future[Result] =
      m(action)

object ActionMatchers extends ActionMatchers

trait FutureMatcher[-T]:

  /** apply this matcher to a value
    * @return
    *   a Future Result describing the outcome of the match
    */
  def apply[S <: T](s: S): Future[Result]

object FutureMatcher:
  def apply[T, R](f: T => Future[R])(using ee: ExecutionEnv): FutureMatcher[T] =
    new FutureMatcher[T]:
      def apply[S <: T](s: S): Future[Result] =
        given ExecutionContext = ee.executionContext
        f(s).map(_ => Success("ok")).recover(t => Error(t))
