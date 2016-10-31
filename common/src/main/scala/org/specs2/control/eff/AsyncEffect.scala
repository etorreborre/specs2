package org.specs2.control
package eff

import all._
import syntax.all._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scalaz._, Scalaz._
import Async._

trait AsyncEffect extends AsyncCreation with AsyncInterpretation
object AsyncEffect extends AsyncEffect

/**
 * Asynchronous effects, using scalaz task as the underlying implementation for now.
 * This can be switch to a Future implementation later.
 */
trait AsyncCreation {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    create[R, A](ec => Future.successful(a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    create[R, A](ec => Future.failed(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    create[R, A](ec => Future(a)(ec))

  def asyncDelayAttempt[R :_async :_throwableOr, A](a: =>A): Eff[R, A] =
    asyncDelay(\/.fromTryCatchNonFatal(a)).collapse

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    asyncDelay(a)

  /** use a ThrowableXor effect to store any exception */
  def asyncForkAttempt[R :_async :_throwableOr, A](a: =>A): Eff[R, A] =
    asyncFork(\/.fromTryCatchNonFatal(a)).collapse

  private def create[R :_async, A](f: ExecutionContext => Future[A]): Eff[R, A] =
    send[Async, R, A](AsyncFuture(f))
}

object AsyncCreation extends AsyncCreation

trait AsyncInterpretation { outer =>

  def runAsyncFuture[A](e: Eff[Fx.fx1[Async], A])(implicit ec: ExecutionContext): Future[A] =
    e.detachA(ApplicativeAsync) match { case AsyncFuture(f) => f(ec) }

  def attempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable \/ A] = {
    e match {
      case Pure(a) => pure[R, Throwable \/ A](\/.right(a))

      case Impure(u, c) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(tx.attempt)

            Impure(union, Arrs.singleton { ex: (Throwable \/ u.X) =>
              ex match {
                case \/-(x) => attempt(c(x))
                case -\/(t) => pure(\/.left(t))
              }
            })

          case None => Impure(u, Arrs.singleton((x: u.X) => attempt(c(x))))
        }

      case ImpureAp(unions, c) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(tx.attempt)
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation = Arrs.singleton[R, List[Any], Throwable \/ A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable \/ Any]
              else \/.right(a)
            }.sequence

          xors match {
            case -\/(t) => pure(\/.left(t))
            case \/-(anys) => attempt(c(anys))
          }
        }

        ImpureAp(materializedUnions, continuation)
    }
  }
}

object AsyncInterpretation extends AsyncInterpretation

sealed trait Async[+A] {
  def attempt: Async[Throwable \/ A]
}
case class AsyncFuture[A](run: ExecutionContext => Future[A]) extends Async[A] {
  def attempt: Async[Throwable \/ A] =
    AsyncFuture { implicit ec =>
      run(ec).map(a => \/.right[Throwable, A](a))(ec) recover { case NonFatal(t) => -\/(t) }
    }
}

/**
 * The Monad instance is necessary to be able to do a detach operation
 * when Async is the last effect of the stack (see runAsyncTask)
 */
object Async {

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {
    def point[A](a: =>A) = AsyncFuture(_ => Future.successful(a))

    def ap[A, B](fa: =>Async[A])(ff: =>Async[A => B]): Async[B] =
      AsyncFuture { implicit ec =>
        (ff, fa) match { case (AsyncFuture(ff1), AsyncFuture(fa1)) =>
          Future.sequence(List(ff1(ec), fa1(ec))).map {
            case f :: a :: _ => f.asInstanceOf[A => B](a.asInstanceOf[A])
            case _           => sys.error("impossible")
          }
        }
      }

    override def toString = "Applicative[Async]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def point[A](a: =>A) = AsyncFuture(_ => Future.successful(a))

    def bind[A, B](fa: Async[A])(f: A => Async[B]): Async[B] =
      AsyncFuture { implicit ec =>
        fa match { case AsyncFuture(fa1) =>
          fa1(ec).flatMap(a => f(a) match { case AsyncFuture(fa2) => fa2(ec) })
        }
      }

    override def toString = "Monad[Async]"
  }
}

