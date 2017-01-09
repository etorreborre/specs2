package org.specs2.control
package eff

import scalaz._, Scalaz._
import all._

import scala.concurrent.duration.FiniteDuration
import SubscribeEffect._

trait AsyncEffect extends AsyncCreation

object AsyncEffect extends AsyncEffect

trait AsyncCreation {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

  def subscribe[R :_async, A](c: Subscribe[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](c), timeout))

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncNow[A](a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    send[Async, R, A](AsyncFailed[A](t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncDelayed[A](Need(a)))

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](SimpleSubscribe((c: Callback[A]) => c(\/.fromTryCatchNonFatal(a).toEither))), timeout))

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    asyncDelay[R, Eff[R, A]] {
      a match {
        case AsyncNow(a1)     => asyncFork(a1, timeout)
        case AsyncDelayed(a1) => asyncFork(a1.value, timeout)
        case AsyncFailed(t)   => asyncFail(t)
        case AsyncEff(e, to)  => send[Async, R, A](AsyncEff(e, to))
      }
    }.flatten

  def async[R :_async, A](subscribe: Subscribe[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](subscribe), timeout))

}

object AsyncCreation extends AsyncCreation

trait AsyncInterpretation {

  def asyncAttempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable \/ A] = {
    e match {
      case Pure(a, last) =>
        pure[R, Throwable \/ A](\/.right(a)).addLast(last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(attempt(tx))

            Impure(union, Arrs.singleton { ex: (Throwable \/ u.X) =>
              ex match {
                case \/-(x) => asyncAttempt(c(x))
                case -\/(t) => pure(\/.left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => asyncAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(attempt(tx).asInstanceOf[Async[Any]])
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable \/ A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable \/ Any]
              else \/.right(a)
            }.sequence

          xors match {
            case -\/(t)     => pure(\/.left(t))
            case \/-(anys) => asyncAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  def attempt[A](a: Async[A]): Async[Throwable \/ A] =
    a match {
      case AsyncNow(a1)     => AsyncNow[Throwable \/ A](\/-(a1))
      case AsyncFailed(t)   => AsyncNow[Throwable \/A](-\/(t))
      case AsyncDelayed(a1) => AsyncDelayed(Need(\/.fromTryCatchNonFatal(a1.value)))
      case AsyncEff(e, to)  => AsyncEff(subscribeAttempt(e), to)
    }

  implicit final def toAttemptOps[R, A](e: Eff[R, A]): AttemptOps[R, A] = new AttemptOps[R, A](e)

}

final class AttemptOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def asyncAttempt(implicit async: Async /= R): Eff[R, Throwable \/ A] =
    AsyncInterpretation.asyncAttempt(e)
}

object AsyncInterpretation extends AsyncInterpretation

sealed trait Async[A] extends Any

case class AsyncNow[A](a: A) extends Async[A]
case class AsyncFailed[A](t: Throwable) extends Async[A]
case class AsyncDelayed[A](a: Name[A]) extends Async[A]
case class AsyncEff[A](e: Eff[FS, A], timeout: Option[FiniteDuration] = None) extends Async[A]

object Async {

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def point[A](a: =>A) = AsyncNow(a)

    def ap[A, B](fa: =>Async[A])(ff: =>Async[A => B]): Async[B] =
      (ff, fa) match {
        case (AsyncNow(f), AsyncNow(a)) =>
          AsyncNow(f(a))

        case (AsyncNow(f), AsyncDelayed(a)) =>
          AsyncDelayed(a.map(f))

        case (AsyncNow(f), AsyncEff(a, to)) =>
          AsyncEff(a.map(f), to)

        case (AsyncDelayed(f), AsyncNow(a)) =>
          AsyncDelayed(Need(f.value(a)))

        case (AsyncDelayed(f), AsyncDelayed(a)) =>
          AsyncDelayed(Apply[Name].ap(a)(f))

        case (AsyncDelayed(f), AsyncEff(a, toa)) =>
          AsyncEff(a.map(f.value), toa)

        case (AsyncEff(f, to), AsyncNow(a)) =>
          AsyncEff(f.map(_(a)), to)

        case (AsyncEff(f, tof), AsyncDelayed(a)) =>
          AsyncEff(f.map(_(a.value)), tof)

        case (_, AsyncFailed(t)) =>
          AsyncFailed(t)

        case (AsyncFailed(t), _) =>
          AsyncFailed(t)

        case (AsyncEff(f, tof), AsyncEff(a, toa)) =>
          AsyncEff(EffApplicative[FS].ap(a)(f), (tof |@| toa)(_ min _))
      }

    override def toString = "Applicative[Async]"
  }

  implicit final def MonadAsync: Monad[Async] with BindRecʹ[Async] = new Monad[Async] with BindRecʹ[Async] {
    def point[A](a: =>A) = AsyncEff(Eff.pure[FS, A](a))

    def bind[A, B](aa: Async[A])(f: A => Async[B]): Async[B] =
      aa match {
        case AsyncNow(a) =>
          f(a)

        case AsyncFailed(t) =>
          AsyncFailed(t)

        case AsyncDelayed(a) =>
          \/.fromTryCatchNonFatal(f(a.value)) match {
            case -\/(t)   => AsyncFailed(t)
            case \/-(ab) => ab
          }

        case AsyncEff(ea, toa) =>
          AsyncEff[B](ea.flatMap(a => subscribeAsync(f(a))), toa)
      }

    def subscribeAsync[A](a: Async[A]): Eff[FS, A] =
      a  match {
        case AsyncNow(a1)     => Eff.pure[FS, A](a1)
        case AsyncFailed(t)   => send[Subscribe, FS, A](SimpleSubscribe(c => c(Left(t))))
        case AsyncDelayed(a1) => send[Subscribe, FS, A](SimpleSubscribe(c => c(\/.fromTryCatchNonFatal(a1.value).toEither)))
        case AsyncEff(a1, to) => a1
      }

    def tailrecM[A, B](a: A)(f: A => Async[A \/ B]): Async[B] =
      f(a) match {
        case AsyncNow(-\/(a1)) => tailrecM(a1)(f)
        case AsyncNow(\/-(b)) => AsyncNow[B](b)
        case AsyncFailed(t)      => AsyncFailed[B](t)
        case AsyncDelayed(ab) =>
          \/.fromTryCatchNonFatal(ab.value) match {
            case -\/(t) => AsyncFailed[B](t)
            case \/-(\/-(b)) => AsyncNow[B](b)
            case \/-(-\/(a1)) => tailrecM(a1)(f)
          }
        case AsyncEff(e, to) =>
          e match {
            case Pure(ab, last) => ab match {
              case -\/(a1) => tailrecM(a1)(f)
              case \/-(b) => AsyncNow[B](b)
            }

            case imp @ Impure(u, c, last) =>
              AsyncEff(imp.flatMap {
                case -\/(a1) => subscribeAsync(tailrecM(a1)(f))
                case \/-(b) => Eff.pure(b)
              }, to)

            case imp @ ImpureAp(unions, continuation, last) =>
              AsyncEff(imp.flatMap {
                case -\/(a1) => subscribeAsync(tailrecM(a1)(f))
                case \/-(b) => Eff.pure(b)
              }, to)
          }
      }

    override def toString = "Monad[Async]"
  }
}


