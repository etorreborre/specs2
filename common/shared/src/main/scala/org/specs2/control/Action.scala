package org.specs2
package control

import fp._, syntax._
import org.specs2.concurrent.{ExecutorServices}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.global
import scala.util._
import scala.annotation.tailrec
import Finalizer._
import Operation._
import execute._

case class Action[A](runNow: ExecutionContext => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty) {

  def attempt: Action[Throwable Either A] =
    Action(ec => runNow(ec).transform(r => scala.util.Success(r.toEither))(ec), timeout, last)

  def runFuture(es: ExecutorServices): Future[A] =
    timeout.fold(runNow(es.executionContext)) { t =>
      val promise = Promise[A]
      es.schedule( { promise.tryFailure(new TimeoutException); () }, t)
      promise.tryCompleteWith(runNow(es.executionContext))
      promise.future
    }

  def runOption(ec: ExecutionContext): Option[A] =
    runAction(ec).toOption

  def runMonoid(ec: ExecutionContext)(implicit m: Monoid[A]): A =
    runOption(ec).getOrElse(m.zero)

  def runAction(ec: ExecutionContext): Throwable Either A =
    try Right(Await.result(runNow(ec), timeout.getOrElse(Duration.Inf)))
    catch { case t: Throwable => Left(t) }
    finally Finalizer.runFinalizers(last)

  def unsafeRunAction(ec: ExecutionContext): A =
    try Await.result(runNow(ec), timeout.getOrElse(Duration.Inf))
    finally Finalizer.runFinalizers(last)

  def toOperation: Operation[A] =
    Operation(() => Await.result(runNow(global), timeout.getOrElse(Duration.Inf)), last)

  def addLast(finalizer: Finalizer): Action[A] =
    copy(last = last :+ finalizer)
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
}

object Action {

  def pure[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def unit: Action[Unit] =
    pure(())

  def protect[A](a: =>A): Action[A] =
    ActionMonad.point(a)

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

  def orElse(alternative: Operation[A]): Operation[A] =
    attempt.flatMap {
      case Left(_) => alternative
      case Right(a) => Operation(() => a, last)
    }

  def attempt: Operation[Throwable Either A] =
    try {
      val value = operation()
      Operation(() => Right(value), last)
    }
    catch { case t: Throwable => Operation(() => Left(t), last) }

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
