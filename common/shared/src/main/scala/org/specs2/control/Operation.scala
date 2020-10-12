package org.specs2
package control

import fp._, syntax._
import execute._
import scala.annotation.tailrec
import scala.util.control.NonFatal

/**
 *
 * Synchronous action with:
 *
 * - an optional list of "finalization" actions to be executed when this action is done
 *   if it throws an exception. This allows resources to be safely disposed of
 *
 * It is essentially the same as an Action without the asynchronicity
 */
case class Operation[A](operation: () => Throwable Either A, last: Vector[Finalizer] = Vector.empty):
  private def run: Throwable Either A =
    operation()

  def map[B](f: A => B): Operation[B] =
   Operation[B](() => run.map(f), last)

  def flatMap[B](f: A => Operation[B]): Operation[B] =
    Operation.fromEither[B] {
      runOperation match {
        case Right(a) => f(a).runOperation
        case Left(t) => Left(t)
      }
    }

  /** run this operation, to get back a result (possibly an exception)
   * and run the finalizers when the operation has been executed
   */
  def runOperation: Throwable Either A =
    val Operation(op, last) = attempt
    val result = op().flatten
    Finalizer.runFinalizers(last)
    result

  def unsafeRun: A =
    runOption.get

  def runOption: Option[A] =
    runOperation.toOption

  def runMonoid(implicit m: Monoid[A]): A =
    runOption.getOrElse(m.zero)

  def runVoid(): Unit =
    runOption; ()

  def addLast(finalizer: Finalizer): Operation[A] =
    copy(last = last :+ finalizer)

  def thenFinally(operation: Operation[A]): Operation[A] =
    addLast(Finalizer(() => operation.runVoid()))

  def orElse(other: Operation[A]): Operation[A] =
    attempt.flatMap {
      case Left(t) =>
        other.copy(last = other.last ++ this.last)
      case Right(a) =>
        Operation(() => Right(a), last)
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
    Operation(() =>
      try Right(run)
      catch {
        case NonFatal(t) => Right(Left(t))
      }, last)

  def toAction: Action[A] =
    Action.either(attempt.run.flatten).copy(last = last)

object Operation:

  def ok[A](a: A): Operation[A] =
    pure(a)

  def delayed[A](a: =>A): Operation[A] =
    pure(a)

  def fail[A](a: Any): Operation[A] =
  exception[A](new Exception(a.toString))

  def exception[A](e: Throwable): Operation[A] =
    Operation[A](() => Left(e))

  def fromEither[A](ea: =>Throwable Either A): Operation[A] =
    Operation(() => ea)

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

  implicit val OperationMonad: Monad[Operation[*]] = new Monad[Operation[*]] {
    def point[A](a: =>A): Operation[A] =
      Operation(() => Right(a))

    def bind[A, B](fa: Operation[A])(f: A => Operation[B]): Operation[B] =
      fa.flatMap(f)

    override def ap[A, B](fa: =>Operation[A])(ff: =>Operation[A => B]): Operation[B] =
      Operation(() => ff.run.flatMap(fa.run.map))

    override def tailrecM[A, B](a: A)(f: A => Operation[Either[A, B]]): Operation[B] =
      Operation[B] { () =>
        @tailrec
        def loop(va: A): Throwable Either B =
          f(va).run match
            case Right(Right(b)) => Right(b)
            case Right(Left(a)) => loop(a)
            case Left(t) => Left(t)
        loop(a)
      }

    override def toString: String =
      "Monad[Operation]"
  }

  implicit val OperationApplicative: Applicative[Operation[*]] = new Applicative[Operation[*]] {
    def point[A](a: =>A): Operation[A] =
      Operation(() => Right(a))

    def ap[A, B](fa: =>Operation[A])(ff: =>Operation[A => B]): Operation[B] =
      Operation(() => ff.run.flatMap(f => fa.run.map(f)))

    override def toString: String =
      "Applicative[Operation]"
  }

  implicit val operationToAction: NaturalTransformation[Operation, Action] =
    new NaturalTransformation[Operation, Action] {
      def apply[A](operation: Operation[A]): Action[A] =
        operation.toAction
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
