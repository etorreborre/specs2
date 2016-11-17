package org.specs2.control
package eff

import scalaz._
import org.specs2.control.eff.syntax.all._
import all._
import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeUnit}

import Async._
import SubscribeEffect._
import AsyncFutureInterpreter._
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.duration.FiniteDuration
import scala.concurrent._

case class AsyncFutureInterpreter(executionEnv: ExecutionEnv) extends AsyncInterpreter[Future] { outer =>

  implicit lazy val executorService: ExecutorService =
    executionEnv.executorService

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    executionEnv.scheduledExecutorService

  implicit lazy val executionContext: ExecutionContext =
    executionEnv.executionContext

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detachA(MonadAsync, ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detach)

  def suspend[R :_async, A](future: =>Future[Eff[R, A]]): Eff[R, A] =
    fromFuture(future).flatten

  def fromFuture[R :_async, A](future: =>Future[A]): Eff[R, A] =
    subscribe[R, A](callback => future.onComplete {
      case scala.util.Success(a) => callback(\/-(a))
      case scala.util.Failure(t) => callback(-\/(t))

    }, None)

  def run[A](r: Async[A]): Future[A] =
    r match {
      case AsyncNow(a)         => Future.successful(a)
      case AsyncFailed(t)      => Future.failed(t)
      case AsyncDelayed(a, to) => withTimeout(\/.fromTryCatchNonFatal(a.value).fold(Future.failed, Future.successful), to)
      case AsyncEff(e, to)     => subscribeToFuture(e, to).detachA(FutureMonad, FutureApplicative)
    }

  def subscribeToFutureNat(timeout: Option[FiniteDuration]) = new (Subscribe ~> Future) {
    def apply[X](subscribe: Subscribe[X]): Future[X] = {
      val promise: Promise[X] = Promise[X]()
      val callback = (ta: Throwable \/ X) =>
        ta match {
          case -\/(t)  => promise.failure(t); ()
          case \/-(a) => promise.success(a); ()
        }

      withTimeout(FutureApplicative.tuple2(Future(subscribe(callback)), promise.future).map(_._2), timeout)
    }
  }

  def subscribeToFuture[A](e: Eff[Fx1[Subscribe], A], timeout: Option[FiniteDuration])(implicit m: Subscribe <= Fx1[Subscribe]): Eff[Fx1[Future], A] =
    interpret.transform[Fx1[Subscribe], Fx1[Future], NoFx, Subscribe, Future, A](e, subscribeToFutureNat(timeout))

  def withTimeout[A](future: =>Future[A], timeout: Option[FiniteDuration]): Future[A] =
    timeout match {
      case None => future
      case Some(to) =>
        lazy val attemptFuture = \/.fromTryCatchNonFatal(future).fold(Future.failed, identity)

        if (to.isFinite && to.length < 1) attemptFuture
        else {
          val p = Promise[A]()
          val r = new Runnable { def run: Unit = { p.completeWith(attemptFuture); () } }
          scheduledExecutorService.schedule(r, to.toMillis, TimeUnit.MILLISECONDS)
          p.future
        }
    }

  implicit final def toRunAsyncFutureOps[A](e: Eff[Fx.fx1[Async], A]): RunAsyncFutureOps[A] =
    new RunAsyncFutureOps[A](e)

  final class RunAsyncFutureOps[A](val e: Eff[Fx.fx1[Async], A]) {

    def runAsyncFuture: Future[A] =
      outer.runAsync(e)

    def runAsyncSequential: Future[A] =
      outer.runSequential(e)
  }

}

trait AsyncInterpreter[F[_]] {

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): F[A]

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): F[A]

}


object AsyncFutureInterpreter {

  def create(implicit ec: ExecutionContext): AsyncFutureInterpreter =
    fromExecutionContext(ec)

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionContext(ec: =>ExecutionContext): AsyncFutureInterpreter =
    fromExecutionEnv(ExecutionEnv.fromExecutionContext(ec))

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionEnv(ee: ExecutionEnv): AsyncFutureInterpreter =
    AsyncFutureInterpreter(ee)

  def FutureApplicative(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def point[A](x: =>A): Future[A] =
      Future.successful(x)

    def ap[A, B](fa: =>Future[A])(ff: =>Future[A => B]): Future[B] = {
      fa.zip(ff).map { case (a, f) => f(a) }
    }

    override def toString = "Applicative[Future]"
  }

  implicit def FutureMonad(implicit ec: ExecutionContext): Monad[Future] with BindRec[Future] = new Monad[Future] with BindRec[Future] {
    def point[A](x: =>A): Future[A] =
      Future.successful(x)

    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)

    def tailrecM[A, B](f: A => Future[A \/ B])(a: A): Future[B] =
      FutureMonad.bind(f(a)) {
        case -\/(a1) => tailrecM(f)(a1)
        case \/-(b) => FutureMonad.point(b)
      }

    override def toString = "Monad[Future]"

  }

}
