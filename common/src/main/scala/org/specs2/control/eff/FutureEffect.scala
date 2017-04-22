package org.specs2.control.eff

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.util.{Failure, Success}
import org.specs2.fp._
import Eff._

object FutureCreation extends FutureCreation

final case class TimedFuture[A](callback: (ScheduledExecutorService, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None) {
  @inline def runNow(sexs: ScheduledExecutorService, exc: ExecutionContext): Future[A] = {
    timeout.fold {
      callback(sexs, exc)
    } { t =>
      val promise = Promise[A]
      val timeout = new Runnable {
        override def run(): Unit = {
          val _ = promise.tryFailure(new TimeoutException)
        }
      }
      sexs.schedule(timeout, t.length, t.unit)
      promise.tryCompleteWith(callback(sexs, exc))
      promise.future
    }
  }

  def attempt: TimedFuture[Throwable Either A] =
    TimedFuture[Throwable Either A](callback = (sexs, ec) => {
      val prom = Promise[Throwable Either A]()
      runNow(sexs, ec).onComplete { t =>
        prom.success(t match {
          case Failure(ex) => Left(ex)
          case Success(v) => Right(v)
        })
      }(ec)
      prom.future
    })

}

object TimedFuture {

  final def ApplicativeTimedFuture: Applicative[TimedFuture] = new Applicative[TimedFuture] {
    def point[A](x: =>A) = TimedFuture((_, _) => Future.successful(x))

    def ap[A, B](fa: =>TimedFuture[A])(ff: =>TimedFuture[(A) => B]): TimedFuture[B] = {
      val newCallback = { (sexs: ScheduledExecutorService, ec: ExecutionContext) =>
        val ffRan = ff.runNow(sexs, ec)
        val faRan = fa.runNow(sexs, ec)
        faRan.flatMap(a => ffRan.map(f => f(a))(ec))(ec)
      }
      TimedFuture(newCallback)
    }
    override def toString = "Applicative[TimedFuture]"
  }

  implicit final def MonadTimedFuture: Monad[TimedFuture] = new Monad[TimedFuture] {
    def point[A](x: => A) = TimedFuture((_, _) => Future.successful(x))

    def bind[A, B](fa: TimedFuture[A])(f: A => TimedFuture[B]): TimedFuture[B] =
      TimedFuture[B]((sexs, ec) => fa.runNow(sexs, ec).flatMap(f(_).runNow(sexs, ec))(ec))

    override def tailrecM[A, B](a: A)(f: A => TimedFuture[Either[A, B]]): TimedFuture[B] =
      TimedFuture[B]({ (sexs, ec) =>
        def loop(va: A): Future[B] = f(va).runNow(sexs, ec).flatMap {
          case Left(na) => loop(na)
          case Right(nb) => Future.successful(nb)
        }(ec)
        loop(a)
      })

    override def toString = "Monad[TimedFuture]"
  }

  def successful[A](a: A): TimedFuture[A] =
    future(Future.successful(a))

  def failed[A](t: Throwable): TimedFuture[A] =
    future(Future.failed(t))

  def future[A](f: =>Future[A]): TimedFuture[A] =
    TimedFuture((_, _) => f)
}

trait FutureTypes {
  type _future[R] = TimedFuture |= R
  type _Future[R] = TimedFuture <= R
}

trait FutureCreation extends FutureTypes {

  final def fromFutureWithExecutors[R :_future, A](c: (ScheduledExecutorService, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture(c, timeout))

  final def futureFail[R :_future, A](t: Throwable): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future.failed(t)))

  final def futureFromEither[R :_future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R :_future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future.successful(a), timeout))

  final def futureFork[R :_future, A](a: => A, ec: ExecutionContext, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future(a)(ec), timeout))

  final def futureDefer[R :_future, A](a: =>Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => a, timeout))

}

trait FutureInterpretation extends FutureTypes {

  def runAsync[R, A](e: Eff[R, A])(implicit sexs: ScheduledExecutorService, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detachA(Eff.effInto[R, Fx1[TimedFuture], A](e))(TimedFuture.MonadTimedFuture, TimedFuture.ApplicativeTimedFuture).runNow(sexs, exc)

  def runSequential[R, A](e: Eff[R, A])(implicit sexs: ScheduledExecutorService, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detach(Eff.effInto[R, Fx1[TimedFuture], A](e)).runNow(sexs, exc)

  import interpret.of

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TimedFuture, Throwable Either ?, A](e,
      new (TimedFuture ~> (TimedFuture of (Throwable Either ?))#l) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[Throwable Either X] = fa.attempt
      })

  final def memoize[A](key: AnyRef, cache: Cache, future: TimedFuture[A]): TimedFuture[A] =
    TimedFuture { (sexs, ec) =>
      val prom = Promise[A]()
      cache.get[A](key).fold {
        prom.completeWith(future.runNow(sexs, ec).map { v => val _ = cache.put(key, v); v }(ec))
      } { v => prom.success(v) }
      prom.future
    }

  /**
   * Memoize future values using a cache
   *
   * if this method is called with the same key the previous value will be returned
   */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, A] =
    interpret.interceptNat[R, TimedFuture, A](e)(
      new (TimedFuture ~> TimedFuture) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[X] = memoize(key, cache, fa)
      }
    )

}

object FutureInterpretation extends FutureInterpretation

trait FutureEffect extends FutureCreation with FutureInterpretation

object FutureEffect extends FutureEffect

