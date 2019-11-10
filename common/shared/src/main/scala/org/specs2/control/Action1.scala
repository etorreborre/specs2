package org.specs2
package control

import org.specs2.concurrent._
import fp._, syntax._
import scala.concurrent._
import scala.concurrent.duration._
import scala.annotation.tailrec
import Action1._

sealed trait Message
case object NoMessage extends Message
case class ConsoleMessage(m: String) extends Message
case class WarningMessage(m: String) extends Message

case class Labelled[A](a: A, messages: Vector[Message] = Vector.empty)

object Labelled {
  implicit val LabelledMonad: Monad[Labelled[?]] = new Monad[Labelled[?]] {
    def point[A](a: =>A): Labelled[A] =
      Labelled(a)

    def bind[A, B](fa: Labelled[A])(f: A => Labelled[B]): Labelled[B] =
      fa match {
        case Labelled(a, messages) =>
          val Labelled(b, messages2) = f(a)
          Labelled(b, messages ++ messages2)
      }

    def ap[A, B](fa: =>Labelled[A])(ff: =>Labelled[A => B]): Labelled[B] = {
      val messages = fa.messages ++ ff.messages
      Labelled(ff.a(fa.a), messages)
    }

    @tailrec
    def tailrecM[A, B](a: A)(f: A => Labelled[Either[A, B]]): Labelled[B] =
      f(a) match {
        case Labelled(Right(b), messages) => Labelled(b, messages)
        case Labelled(Left(a), messages) => tailrecM(a)(f)
      }
    def toString = "Monad[Labelled]"
  }
}

trait ExecutionIssue
case class ThrowableIssue(t: Throwable) extends ExecutionIssue
case class FailureIssue(t: String) extends ExecutionIssue
case class FinalizationIssue(t: Throwable) extends ExecutionIssue

case class Action1[A](runNow: ExecutorServices => Future[Execute[A]], timeout: Option[FiniteDuration] = None)

object Action1 {
  type Execute[A] = Either[ExecutionIssue, Labelled[A]]

  def pure[A](a: =>A): Action1[A] =
    Action1Monad.point(a)

  def unit: Action1[Unit] =
    pure(())

  def protect[A](a: =>A): Action1[A] =
    Action1Monad.point(a)

  implicit val Action1Monad: Monad[Action1[?]] = new Monad[Action1[?]] {
    def point[A](a: =>A): Action1[A] =
      Action1(_ => Future.successful(Right(Labelled(a))))
    def bind[A, B](fa: Action1[A])(f: A => Action1[B]): Action1[B] =
      Action1[B] { es =>
        implicit val ec: ExecutionContext = es.executionContext
        fa.runNow(es).flatMap {
          case Left(e) =>
            Future.successful(Left(e))
          case Right(Labelled(a, messages1)) =>
            f(a).runNow(es).map { r =>
              r.map {
                case Labelled(b, messages2) => Labelled(b, messages1 ++ messages2)
              }
            }
        }
    }

    def ap[A, B](fa: =>Action1[A])(ff: =>Action1[A => B]): Action1[B] = {
      Action1 { es: ExecutorServices =>
        implicit val ec: ExecutionContext = es.executionContext
        fa.runNow(es).zip(ff.runNow(es)).map {
          case (Left(e), _) => Left(e)
          case (_, Left(e)) => Left(e)
          case (Right(l1), Right(l2)) => Right(l1.ap(l2))
        }
      }
    }

    def tailrecM[A, B](a: A)(f: A => Action1[Either[A, B]]): Action1[B] =
      Action1[B] { es =>
        implicit val ec: ExecutionContext = es.executionContext
        def loop(va: A, ms1: Vector[Message] = Vector.empty): Future[Execute[B]] =
          f(va).runNow(es).flatMap {
            case Left(e) => Future.successful(Left(e))
            case Right(Labelled(Right(b), ms2)) => Future.successful(Right(Labelled(b, ms1 ++ ms2)))
            case Right(Labelled(Left(a), ms2)) => loop(a, ms1 ++ ms2)
          }
        loop(a)
      }
    def toString = "Monad[Action1]"
  }
}

case class Operation1[A](operation: Execute[A])
