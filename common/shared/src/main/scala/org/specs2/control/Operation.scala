package org.specs2
package control

import fp._, syntax._
import execute._
import scala.annotation.tailrec

/**
 *
 * Synchronous action with:
 *
 * - an optional list of "finalization" actions to be executed when this action is done
 *   whether it has timed-out or thrown an exception. This allows resources to be safely disposed of
 *
 * It is essentially the same as an Action without the asynchronicity
 */
case class Operation[A](operation: () => A, last: Vector[Finalizer] = Vector.empty) {
  private def run: A =
    operation()

  def runOperation: Throwable Either A = {
    val attempted = attempt
    Finalizer.runFinalizers(attempted.last)
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
