package org.specs2
package control

import fp._, syntax._
import concurrent.{ExecutionEnv, ExecuteActions}
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._
import execute._

/**
 * Asynchronous action with:
 *
 * - an optional timeout
 * - an optional list of "finalization" actions to be executed when this action is done
 *   whether it has timed-out or thrown an exception. This allows resources to be safely disposed of
 *
 */
case class Action[A](runNow: ExecutionContext => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty) {

  /** add a finalizer */
  def addLast(finalizer: Finalizer): Action[A] =
    copy(last = last :+ finalizer)

  /** catch any exception resulting from running the action later */
  def attempt: Action[Throwable Either A] =
    Action(ec => runNow(ec).transform(r => scala.util.Success(r.toEither))(ec), timeout, last)

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
    ExecuteActions.runActionToFuture(runNow, timeout, ee)

  /**
   * Run the action and return an exception if it fails
   * Whatever happens run the finalizers
   */
  def runAction(ee: ExecutionEnv): Throwable Either A =
    ExecuteActions.awaitAction(runNow, timeout, Finalizer.runFinalizers(last), ee)

  /** run the action and return Nothing is case of an error */
  def runOption(ee: ExecutionEnv): Option[A] =
    runAction(ee).toOption

  /** run the action for its side effects */
  def runVoid(ee: ExecutionEnv): Unit =
    runOption(ee).void.getOrElse(())

  /** run the action and the return an empty value in case of an error */
  def runMonoid(ee: ExecutionEnv)(implicit m: Monoid[A]): A =
    runOption(ee).getOrElse(m.zero)

  /** run the action and throw any exception */
  def unsafeRunAction(ee: ExecutionEnv): A =
    runOption(ee).get

  // for backwards compatibility
  def run(ee: ExecutionEnv): A =
    unsafeRunAction(ee)
}


object Action {

  def pure[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def unit: Action[Unit] =
    pure(())

  def protect[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def fail[A](message: String): Action[A] =
    exception(new UserException(message, new Exception))

  def exception[A](t: Throwable): Action[A] =
    Action(_ => Future.failed[A](t))

  def future[A](f: Future[A]): Action[A] =
    Action(_ => f)

  def attempt[A](action: =>Action[A]): Action[Either[Throwable, A]] =
    Action { implicit es =>
      action.runNow(es).map(Right.apply).recoverWith { case e => Future.successful(Left(e)) }
    }

  def thenFinally[A](action: Action[A], last: Finalizer): Action[A] =
    action.addLast(last)

  def checkThat[A](a: =>A, condition: Boolean, failureMessage: String): Action[A] =
    pure(a).flatMap { value =>
      if (condition) pure(value)
      else           fail(failureMessage)
    }

  implicit val ActionMonad: Monad[Action[?]] = new Monad[Action[?]] {
    def point[A](a: =>A): Action[A] =
      Action(_ => Future.successful(a))

    def bind[A, B](fa: Action[A])(f: A => Action[B]): Action[B] =
      Action[B] { implicit es =>
        fa.runNow(es).flatMap { case a => f(a).runNow(es) }
    }

    override def ap[A, B](fa: =>Action[A])(ff: =>Action[A => B]): Action[B] = {
      Action { implicit ec =>
        fa.runNow(ec).zip(ff.runNow(ec)).map { case (a, f) => f(a) }
      }
    }

    override def toString: String =
      "Monad[Action]"
  }

  implicit val ActionApplicative: Applicative[Action[?]] = new Applicative[Action[?]] {
    def point[A](a: =>A): Action[A] =
      Action(_ => Future.successful(a))

    def ap[A, B](fa: =>Action[A])(ff: =>Action[A => B]): Action[B] = {
      Action { implicit ec =>
        fa.runNow(ec).zip(ff.runNow(ec)).map { case (a, f) => f(a) }
      }
    }

    override def toString: String =
      "Applicative[Action]"
  }

  implicit def FinalizedAction: Safe[Action] = new Safe[Action] {
    def finalizeWith[A](fa: Action[A], f: Finalizer): Action[A] =
      fa.addLast(f)

    def attempt[A](action: Action[A]): Action[Throwable Either A] =
      action.attempt

  }

  implicit def actionAsResult[T : AsResult]: AsResult[Action[T]] = new AsResult[Action[T]] {
    def asResult(action: =>Action[T]): Result =
      action.runAction(ExecutionEnv.fromGlobalExecutionContext).fold(err => Error(err),  ok => AsResult(ok))
  }

}
