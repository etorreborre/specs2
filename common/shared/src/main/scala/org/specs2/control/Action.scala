package org.specs2
package control

import fp.*, syntax.*
import concurrent.{ExecutionEnv, *}
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.control.NonFatal
import scala.util.*
import execute.*

/**
 * Asynchronous action with:
 *
 * - an optional timeout
 * - an optional list of "finalization" actions to be executed when this action is done
 *   whether it has timed-out or thrown an exception. This allows resources to be safely disposed of
 *
 */
case class Action[A](private[control] runNow: ExecutionEnv => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty):

  def map[B](f: A => B): Action[B] =
    Action[B](
      runNow = ee => this.runNow(ee).map(f)(using ee.executionContext),
      timeout = timeout,
      last = last)

  def flatMap[B](f: A => Action[B]): Action[B] =
    var otherTimeout: Option[FiniteDuration] = None
    var otherLast: Vector[Finalizer] = Vector.empty

    Action[B](
      runNow = ee => {
        given ExecutionContext = ee.executionContext
        runFuture(ee).flatMap { a =>
          val otherAction = f(a)
          otherTimeout = otherAction.timeout
          otherLast = otherAction.last
          otherAction.runNow(ee)
        }},
      timeout = otherTimeout,
      last = last ++ otherLast)

  /** add a finalizer */
  def addLast(finalizer: Finalizer): Action[A] =
    copy(last = last :+ finalizer)

  /** add a finalizer */
  def thenFinally(last: Finalizer): Action[A] =
    addLast(last)

  /** catch any exception resulting from running the action later */
  def attempt: Action[Throwable `Either` A] =
    Action(ee => runFuture(ee).transform(r => scala.util.Success(r.toEither))(ee.executionContext), timeout, last)

  /** run another action if this one fails */
  def orElse(other: Action[A]): Action[A] =
    attempt.flatMap {
      case Left(_) => other.copy(last = other.last ++ this.last)
      case Right(a) => Action.pure(a).copy(timeout = timeout, last = last)
    }

  /** synonym for orElse */
  def |||(other: Action[A]): Action[A] =
    orElse(other)

  /**
   * run as a Future and raise a timeout exception if necessary
   * NOTE: this does not execute the finalizers!!!
   */
  def runFuture(ee: ExecutionEnv): Future[A] =
    runActionToFuture(runNow, timeout, ee)

  /**
   * Run the action and return an exception if it fails
   * Whatever happens run the finalizers
   */
  def runAction(ee: ExecutionEnv): Throwable `Either` A =
    awaitAction(runNow, timeout, Finalizer.runFinalizers(last), ee)

  /** run the action and return Nothing is case of an error */
  def runOption(ee: ExecutionEnv): Option[A] =
    runAction(ee).toOption

  /** run the action for its side effects */
  def runVoid(ee: ExecutionEnv): Unit =
    runOption(ee).void.getOrElse(())

  /** run the action and the return an empty value in case of an error */
  def runMonoid(ee: ExecutionEnv)(using m: Monoid[A]): A =
    runOption(ee).getOrElse(m.zero)

  /** run the action and throw any exception */
  def unsafeRunAction(ee: ExecutionEnv): A =
    runOption(ee).get

  // for backwards compatibility
  def run(ee: ExecutionEnv): A =
    unsafeRunAction(ee)


object Action:

  def pure[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def unit: Action[Unit] =
    pure(())

  def protect[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def fail[A](message: String): Action[A] =
    exception(new UserException(message, new Exception))

  def either[A](ta: =>Throwable `Either` A): Action[A] =
    try
        ta match {
          case Left(t) => Action.exception(t)
          case Right(a) => Action.pure(a)
        }
    catch {
      case NonFatal(t) => Action.exception(t)
    }

  def exception[A](t: Throwable): Action[A] =
    Action(_ => Future.failed[A](t))

  def future[A](f: Future[A], timeout: Option[FiniteDuration] = None): Action[A] =
    Action(_ => f, timeout = timeout)

  def checkThat[A](a: =>A, condition: Boolean, failureMessage: String): Action[A] =
    pure(a).flatMap { value =>
      if condition then pure(value)
      else           fail(failureMessage)
    }

  given ActionMonad: Monad[Action[*]] with
    def point[A](a: =>A): Action[A] =
      Action(_ => Future.successful(a))

    def bind[A, B](fa: Action[A])(f: A => Action[B]): Action[B] =
      fa.flatMap(f)

    override def ap[A, B](fa: =>Action[A])(ff: =>Action[A => B]): Action[B] =
      Action { ee =>
        given ExecutionContext = ee.executionContext
        fa.runNow(ee).zip(ff.runNow(ee)).map { case (a, f) => f(a) }
      }

    override def toString: String =
      "Monad[Action]"

  given ActionApplicative: Applicative[Action[*]] with
    def point[A](a: =>A): Action[A] =
      Action(_ => Future.successful(a))

    def ap[A, B](fa: =>Action[A])(ff: =>Action[A => B]): Action[B] =
      Action { ee =>
        given ExecutionContext = ee.executionContext
        fa.runNow(ee).zip(ff.runNow(ee)).map { case (a, f) => f(a) }
      }

    override def toString: String =
      "Applicative[Action]"

  given NaturalTransformation[Id, Action] =
    NaturalTransformation.naturalId[Action]

  given FinalizedAction: Safe[Action]with
    def finalizeWith[A](fa: Action[A], f: Finalizer): Action[A] =
      fa.addLast(f)

    def attempt[A](action: Action[A]): Action[Throwable `Either` A] =
      action.attempt

  given actionAsResult[T : AsResult]: AsResult[Action[T]] with
    def asResult(action: =>Action[T]): Result =
      action.runAction(ExecutionEnv.fromGlobalExecutionContext).fold(err => Error(err),  ok => AsResult(ok))
