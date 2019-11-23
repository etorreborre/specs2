package org.specs2
package control

import org.specs2.concurrent._
import fp._
import scala.concurrent._
import scala.concurrent.duration._
import scala.annotation.tailrec

case class Action1[A](runNow: ExecutorServices => Future[A], timeout: Option[FiniteDuration] = None, last: Vector[Finalizer] = Vector.empty) {
  def addLast(finalizer: Finalizer): Action1[A] =
    copy(last = last :+ finalizer)
}

case class Finalizer(run: () => Unit)

object Action1 {

  def pure[A](a: =>A): Action1[A] =
    Action1Monad.point(a)

  def unit: Action1[Unit] =
    pure(())

  def protect[A](a: =>A): Action1[A] =
    Action1Monad.point(a)

  def attempt[A](action: =>Action1[A]): Action1[Either[Throwable, A]] =
    Action1 { es =>
      implicit val ec: ExecutionContext = es.executionContext
      action.runNow(es).map(Right.apply).recoverWith { case e => Future.successful(Left(e)) }
    }

  def thenFinally[A](action: Action1[A], last: Finalizer): Action1[A] =
    action.addLast(last)

  implicit val Action1Monad: Monad[Action1[?]] = new Monad[Action1[?]] {
    def point[A](a: =>A): Action1[A] =
      Action1(_ => Future.successful(a))

    def bind[A, B](fa: Action1[A])(f: A => Action1[B]): Action1[B] =
      Action1[B] { es =>
        implicit val ec: ExecutionContext = es.executionContext
        fa.runNow(es).flatMap { case a => f(a).runNow(es) }
    }

    override def ap[A, B](fa: =>Action1[A])(ff: =>Action1[A => B]): Action1[B] = {
      Action1 { es: ExecutorServices =>
        implicit val ec: ExecutionContext = es.executionContext
        fa.runNow(es).zip(ff.runNow(es)).map { case (a, f) => f(a) }
      }
    }

    override def toString: String =
      "Monad[Action1]"
  }

  implicit val Action1Applicative: Applicative[Action1[?]] = new Applicative[Action1[?]] {
    def point[A](a: =>A): Action1[A] =
      Action1(_ => Future.successful(a))

    def ap[A, B](fa: =>Action1[A])(ff: =>Action1[A => B]): Action1[B] = {
      Action1 { es: ExecutorServices =>
        implicit val ec: ExecutionContext = es.executionContext
        fa.runNow(es).zip(ff.runNow(es)).map { case (a, f) => f(a) }
      }
    }

    override def toString: String =
      "Applicative[Action1]"
  }

}

case class Operation1[A](operation: () => A, last: Vector[Finalizer] = Vector.empty) {
  def run: A =
    operation()

  def addLast(finalizer: Finalizer): Operation1[A] =
    copy(last = last :+ finalizer)
}

object Operation1 {

  def pure[A](a: =>A): Operation1[A] =
    Operation1Monad.point(a)

  def unit: Operation1[Unit] =
    pure(())

  def protect[A](a: =>A): Operation1[A] =
    Operation1Monad.point(a)

  def attempt[A](operation: =>Operation1[A]): Operation1[Either[Throwable, A]] =
    Operation1 { () =>
      try Right(operation.run)
      catch { case e: Throwable => Left(e) }
    }

  def thenFinally[A](operation: Operation1[A], last: Finalizer): Operation1[A] =
    operation.addLast(last)

  implicit val Operation1Monad: Monad[Operation1[?]] = new Monad[Operation1[?]] {
    def point[A](a: =>A): Operation1[A] =
      Operation1(() => a)

    def bind[A, B](fa: Operation1[A])(f: A => Operation1[B]): Operation1[B] =
      Operation1[B](() => f(fa.run).run)

    override def ap[A, B](fa: =>Operation1[A])(ff: =>Operation1[A => B]): Operation1[B] =
      Operation1(() => ff.run(fa.run))

   override def tailrecM[A, B](a: A)(f: A => Operation1[Either[A, B]]): Operation1[B] =
      Operation1[B] { () =>
        @tailrec
        def loop(va: A): B =
          f(va).run match {
            case Right(b) => b
            case Left(a) => loop(a)
          }
        loop(a)
      }

    override def toString: String =
      "Monad[Operation1]"
  }

  implicit val Operation1Applicative: Applicative[Operation1[?]] = new Applicative[Operation1[?]] {
    def point[A](a: =>A): Operation1[A] =
      Operation1(() => a)

    def ap[A, B](fa: =>Operation1[A])(ff: =>Operation1[A => B]): Operation1[B] =
      Operation1(() => ff.run(fa.run))

    override def toString: String =
      "Applicative[Operation1]"
  }

}
