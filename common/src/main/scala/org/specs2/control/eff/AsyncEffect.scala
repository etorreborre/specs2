package org.specs2.control
package eff

import all._
import syntax.all._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scalaz._, Scalaz._
import Async._
import org.specs2.concurrent.FutureApplicative

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
    send[Async, R, A](AsyncNow[A](a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    send[Async, R, A](AsyncFailed(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncDelayed(() => a))

  def asyncDelayAttempt[R :_async :_throwableOr, A](a: =>A): Eff[R, A] =
    asyncDelay(\/.fromTryCatchNonFatal(a)).collapse

  def asyncFork[R :_async, A](a: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    send[Async, R, A](AsyncFuture(ec, { ec1 => Future(a)(ec1) }))

  /** use a ThrowableXor effect to store any exception */
  def asyncForkAttempt[R :_async :_throwableOr, A](a: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    asyncFork(\/.fromTryCatchNonFatal(a)).collapse

}

object AsyncCreation extends AsyncCreation

trait AsyncInterpretation { outer =>

  def runAsyncFuture[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncNow(a)          => Future.successful(a)
      case AsyncDelayed(a)      => \/.fromTryCatchNonFatal(a()).fold(Future.failed, Future.successful)
      case AsyncFailed(t)       => Future.failed(t)
      case AsyncFuture(ec, run) => run(ec)
    }

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

case class AsyncNow[A](a: A) extends Async[A] {
  def attempt: Async[Throwable \/ A] =
    AsyncNow(\/-(a))
}

case class AsyncFailed[A](t: Throwable) extends Async[A] {
  def attempt: Async[Throwable \/ A] =
    AsyncNow(-\/(t))
}

case class AsyncDelayed[A](run: () => A) extends Async[A] {
  def attempt: Async[Throwable \/ A] =
    AsyncDelayed(() => \/.fromTryCatchNonFatal(run()))
}

case class AsyncFuture[A](ec: ExecutionContext, run: ExecutionContext => Future[A]) extends Async[A] {
  def attempt: Async[Throwable \/ A] =
    AsyncFuture(ec, { implicit ec =>
      run(ec).map(a => \/.right[Throwable, A](a))(ec) recover { case NonFatal(t) => -\/(t) }
    })
}

/**
 * The Monad instance is necessary to be able to do a detach operation
 * when Async is the last effect of the stack (see runAsyncTask)
 */
object Async {

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def point[A](a: =>A) = AsyncDelayed(() => a)

    def ap[A, B](fa: =>Async[A])(ff: =>Async[A => B]): Async[B] =
      fa match {
        case AsyncNow(a) =>
          ff match {
            case AsyncNow(f)         => AsyncNow(f(a))
            case AsyncFailed(t)      => AsyncFailed(t)
            case AsyncDelayed(f)     => AsyncDelayed(() => f()(a))
            case AsyncFuture(ecf, f) => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a))})
          }

        case AsyncFailed(t) => AsyncFailed(t)

        case AsyncDelayed(a) =>
          ff match {
            case AsyncNow(f)         => AsyncDelayed(() => f(a()))
            case AsyncFailed(t)      => AsyncFailed(t)
            case AsyncDelayed(f)     => AsyncDelayed(() => f()(a()))
            case AsyncFuture(ecf, f) => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a()))})
          }

        case AsyncFuture(eca, a) =>
          AsyncFuture(eca, { implicit ec =>
            val app = FutureApplicative.ApplicativeFuture
            ff match {
              case AsyncNow(f)         => a(ec).map(f)
              case AsyncFailed(t)      => Future.failed(t)
              case AsyncDelayed(f)     => a(ec).map(x => f()(x))
              case AsyncFuture(ecf, f) => app.ap(a(ec))(f(ecf))
            }
          })
      }

    override def toString = "Applicative[Async]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def point[A](a: =>A) = AsyncDelayed(() => a)

    def bind[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncNow(a)     => af(a)
        case AsyncDelayed(a) => af(a())
        case AsyncFailed(t)  => AsyncFailed(t)

        case AsyncFuture(eca, fa) =>
          AsyncFuture(eca, { implicit ec =>
            fa(ec).flatMap { a =>
              af(a) match {
                case AsyncNow(b)          => Future.successful(b)
                case AsyncFailed(t)       => Future.failed(t)
                case AsyncDelayed(b)      => try Future(b()) catch { case NonFatal(t) => Future.failed(t) }
                case AsyncFuture(ecb, fb) => fb(ecb)
              }
            }})
      }

    override def toString = "Monad[Async]"
  }
}

