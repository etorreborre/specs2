package org.specs2.control
package eff

import scalaz._
import org.specs2.control.eff.syntax.all._
import all._
import AsyncFutureService._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

case class AsyncFutureService(ecEval: Name[ExecutionContext]) extends AsyncService {

  implicit val ec: ExecutionContext = ecEval.value

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncFutureNow[A](a))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    send[Async, R, Unit](AsyncFutureFailed(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncFutureDelayed(() => a))

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    fork(AsyncFuture(ec, {ec1 => Future(a)(ec1)}))

  def fork[R :_async, A](a: =>Async[A]): Eff[R, A] =
    asyncDelay(send(a)).flatten

}

trait AsyncFutureServiceInterpretation {

  def runAsyncFuture[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncFutureNow(a)     => Future.successful(a)
      case AsyncFutureDelayed(a) => \/.fromTryCatchNonFatal(a()).fold(Future.failed, Future.successful)
      case AsyncFutureFailed(t)  => Future.failed(t)
      case AsyncFuture(ec, run)  => run(ec)
    }

  implicit class RunAsyncFutureOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncFuture: Future[A] =
      AsyncFutureServiceInterpretation.runAsyncFuture(e)
  }

}

object AsyncFutureServiceInterpretation extends AsyncFutureServiceInterpretation

object AsyncFutureService {

  def create(implicit ec: ExecutionContext): AsyncFutureService =
    fromExecutionContext(ec)

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionContext(ec: =>ExecutionContext): AsyncFutureService =
    AsyncFutureService(Need(ec))

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def point[A](a: =>A) = AsyncFutureNow(a)

    def ap[A, B](fa: =>Async[A])(ff: =>Async[A => B]): Async[B] =
      fa match {
        case AsyncFutureNow(a) =>
          ff match {
            case AsyncFutureNow(f)     => AsyncFutureNow(f(a))
            case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
            case AsyncFutureDelayed(f) => AsyncFutureDelayed(() => f()(a))
            case AsyncFuture(ecf, f)   => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a))})
          }

        case AsyncFutureFailed(t) => AsyncFutureFailed(t)

        case AsyncFutureDelayed(a) =>
          ff match {
            case AsyncFutureNow(f)     => AsyncFutureDelayed(() => f(a()))
            case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
            case AsyncFutureDelayed(f) => AsyncFutureDelayed(() => f()(a()))
            case AsyncFuture(ecf, f)   => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a()))})
          }

        case AsyncFuture(eca, a) =>
          AsyncFuture(eca, { implicit ec =>
            val app = FutureApplicative
            ff match {
              case AsyncFutureNow(f)     => a(ec).map(f)
              case AsyncFutureFailed(t)  => Future.failed(t)
              case AsyncFutureDelayed(f) => a(ec).map(x => f()(x))
              case AsyncFuture(ecf, f)   => app.ap(a(ec))(f(ecf))
            }
          })
      }

    override def toString = "Applicative[AsyncFuture]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def point[A](a: =>A) = AsyncFutureNow(a)

    def bind[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncFutureNow(a)     => af(a)
        case AsyncFutureDelayed(a) => af(a())
        case AsyncFutureFailed(t)  => AsyncFutureFailed(t)

        case AsyncFuture(eca, fa) =>
          AsyncFuture(eca, { implicit ec =>
            fa(ec).flatMap { a =>
              af(a) match {
                case AsyncFutureNow(b)     => Future.successful(b)
                case AsyncFutureFailed(t)  => Future.failed(t)
                case AsyncFutureDelayed(b) => try Future(b()) catch { case NonFatal(t) => Future.failed(t) }
                case AsyncFuture(ecb, fb)  => fb(ecb)
              }
            }})
      }

    override def toString = "Monad[AsyncFuture]"
  }

  def FutureApplicative(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def point[A](x: =>A): Future[A] =
      Future.successful(x)

    def ap[A, B](fa: =>Future[A])(ff: =>Future[A => B]): Future[B] = {
      fa.zip(ff).map { case (a, f) => f(a) }
    }
  }

}

case class AsyncFutureNow[A](a: A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureNow(Right(a))
}

case class AsyncFutureFailed[A](t: Throwable) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureNow(Left(t))
}

case class AsyncFutureDelayed[A](run: () => A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureDelayed(() => \/.fromTryCatchNonFatal(run()).fold(Left(_): Throwable Either A, Right(_): Throwable Either A))
}

case class AsyncFuture[A](ec: ExecutionContext, run: ExecutionContext => Future[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFuture(ec, { implicit ec =>
      run(ec).map(a => Right[Throwable, A](a))(ec) recover { case NonFatal(t) => Left(t) }
    })
}
