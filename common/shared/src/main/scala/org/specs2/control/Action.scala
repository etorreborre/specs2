package org.specs2
package control

import fp._, syntax._
import concurrent.{ExecutionEnv}
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._
import scala.annotation.tailrec
import Finalizer._
import Operation._
import execute._

case class Action[A](runNow: ExecutionContext => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty) {

  /** add a finalizer */
  def addLast(finalizer: Finalizer): Action[A] =
    copy(last = last :+ finalizer)

  /** catch any exception resulting from running the action later */
  def attempt: Action[Throwable Either A] =
    Action(ec => runNow(ec).transform(r => scala.util.Success(r.toEither))(ec), timeout, last)

  def orElse(other: Action[A]): Action[A] =
    attempt.flatMap {
      case Left(_) => other.copy(last = other.last ++ this.last)
      case Right(a) => Action.pure(a).copy(timeout = timeout, last = last)
    }

  def |||(other: Action[A]): Action[A] =
    orElse(other)

  /**
   * run as a Future and raise a timeout exception if necessary
   * NOTE: this does not execute the finalizers!!!
   */
  def runFuture(ee: ExecutionEnv): Future[A] =
    timeout.fold(runNow(ee.executionContext)) { t =>
      val promise = Promise[A]
      ee.executorServices.schedule( { promise.tryFailure(new TimeoutException); () }, t * ee.timeFactor.toLong)
      promise.tryCompleteWith(runNow(ee.executionContext))
      promise.future
    }

  /**
   * Run the action and return an exception if it fails
   * Whatever happens run the finalizers
   */
  def runAction(ee: ExecutionEnv): Throwable Either A =
    try Right(Await.result(runFuture(ee), timeout.getOrElse(Duration.Inf)))
    catch { case t: Throwable => Left(t) }
    finally Finalizer.runFinalizers(last)

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

case class Finalizer(run: () => Unit) {
  def attempt: Option[Throwable] =
    try { run(); None }
    catch { case t: Throwable => Some(t) }
}

trait Safe[F[_]] {
  def finalizeWith[A](fa: F[A], f: Finalizer): F[A]
  def attempt[A](fa: F[A]): F[Throwable Either A]
}

object Safe {
  def apply[F[_]](implicit f: Safe[F]): Safe[F] =
    f
}

object Finalizer {
  def runFinalizers(finalizers: Vector[Finalizer]): Unit =
    finalizers.foreach(_.attempt)

  def create(action: =>Unit): Finalizer =
    Finalizer(() => action)
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

}

case class Operation[A](operation: () => A, last: Vector[Finalizer] = Vector.empty) {
  private def run: A =
    operation()

  def runOperation: Throwable Either A = {
    val attempted = attempt
    runFinalizers(attempted.last)
    attempted.run
  }

  def unsafeRun: A =
    runOption.get

  def runOption: Option[A] =
    runOperation.toOption

  def runMonoid(implicit m: Monoid[A]): A =
    runOption.getOrElse(m.zero)

  def runVoid(): Unit = {
    runOption; ()
  }

  def addLast(finalizer: Finalizer): Operation[A] =
    copy(last = last :+ finalizer)

  def thenFinally(operation: Operation[A]): Operation[A] =
    addLast(Finalizer(() => operation.runVoid))

  def orElse(other: Operation[A]): Operation[A] =
    attempt.flatMap {
      case Left(_) => other.copy(last = other.last ++ this.last)
      case Right(a) => Operation(() => a, last)
    }

  def |||(other: Operation[A]): Operation[A] =
    orElse(other)

  def recoverWith(f: Throwable => A): Operation[A] =
    recover(t => Operation.ok(f(t)))

  def recover(f: Throwable => Operation[A]): Operation[A] =
    attempt.flatMap {
      case Left(t) => f(t)
      case Right(a) => Operation.ok(a)
    }

  def attempt: Operation[Throwable Either A] =
    try {
      val value = operation()
      Operation(() => Right(value), last)
    }
    catch { case t: Throwable => Operation(() => Left(t), last) }

  def toAction: Action[A] =
    Action.pure(run).copy(last = last)
}

object Operation {

  def ok[A](a: A): Operation[A] =
    pure(a)

  def delayed[A](a: =>A): Operation[A] =
    pure(a)

  def fail[A](a: Any): Operation[A] =
  exception[A](new Exception(a.toString))

  def exception[A](e: Throwable): Operation[A] =
    Operation[A](() => throw e)

  def pure[A](a: =>A): Operation[A] =
    OperationMonad.point(a)

  def unit: Operation[Unit] =
    pure(())

  def protect[A](a: =>A): Operation[A] =
    OperationMonad.point(a)

  def attempt[A](operation: =>Operation[A]): Operation[Either[Throwable, A]] =
    Operation { () =>
      try Right(operation.run)
      catch { case e: Throwable => Left(e) }
    }

  def thenFinally[A](operation: Operation[A], last: Finalizer): Operation[A] =
    operation.addLast(last)

  implicit val OperationMonad: Monad[Operation[?]] = new Monad[Operation[?]] {
    def point[A](a: =>A): Operation[A] =
      Operation(() => a)

    def bind[A, B](fa: Operation[A])(f: A => Operation[B]): Operation[B] =
      Operation[B](() => f(fa.run).run)

    override def ap[A, B](fa: =>Operation[A])(ff: =>Operation[A => B]): Operation[B] =
      Operation(() => ff.run(fa.run))

   override def tailrecM[A, B](a: A)(f: A => Operation[Either[A, B]]): Operation[B] =
      Operation[B] { () =>
        @tailrec
        def loop(va: A): B =
          f(va).run match {
            case Right(b) => b
            case Left(a) => loop(a)
          }
        loop(a)
      }

    override def toString: String =
      "Monad[Operation]"
  }

  implicit val OperationApplicative: Applicative[Operation[?]] = new Applicative[Operation[?]] {
    def point[A](a: =>A): Operation[A] =
      Operation(() => a)

    def ap[A, B](fa: =>Operation[A])(ff: =>Operation[A => B]): Operation[B] =
      Operation(() => ff.run(fa.run))

    override def toString: String =
      "Applicative[Operation]"
  }

  implicit def SafeOperation: Safe[Operation] = new Safe[Operation] {
    def finalizeWith[A](fa: Operation[A], f: Finalizer): Operation[A] =
      fa.addLast(f)

    def attempt[A](fa: Operation[A]): Operation[Throwable Either A] =
      fa.attempt
  }

  implicit def operationAsResult[T : AsResult]: AsResult[Operation[T]] = new AsResult[Operation[T]] {
    def asResult(operation: =>Operation[T]): Result =
      operation.runOperation.fold(err => Error(err),  ok => AsResult(ok))
  }
}
