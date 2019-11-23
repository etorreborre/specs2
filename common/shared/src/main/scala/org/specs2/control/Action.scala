package org.specs2
package control

import fp._
import scala.concurrent._
import scala.concurrent.duration._
import scala.annotation.tailrec

case class Action[A](runNow: ExecutionContext => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty) {
  def addLast(finalizer: Finalizer): Action[A] =
    copy(last = last :+ finalizer)
}

case class Finalizer(run: () => Unit)

object Action {

  def pure[A](a: =>A): Action[A] =
    ActionMonad.point(a)

  def unit: Action[Unit] =
    pure(())

  def protect[A](a: =>A): Action[A] =
    ActionMonad.point(a)

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

}

case class Operation[A](operation: () => A, last: Vector[Finalizer] = Vector.empty) {
  def run: A =
    operation()

  def addLast(finalizer: Finalizer): Operation[A] =
    copy(last = last :+ finalizer)
}

object Operation {

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

}
