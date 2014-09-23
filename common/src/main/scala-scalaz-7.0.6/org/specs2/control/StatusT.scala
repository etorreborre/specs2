package org.specs2
package control

import scalaz.{\/, \&/, Equal, Monad, Functor}, \&/._
import scalaz.syntax.monad._
import scalaz.effect._

/**
 * Transformer version of Status
 *
 * Credits to @markhibberd
 */
case class StatusT[F[+_], +A](run: F[Status[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): StatusT[F, B] =
    StatusT(run.map(_.map(f)))

  def flatMap[B](f: A => StatusT[F, B])(implicit F: Monad[F]): StatusT[F, B] =
    StatusT(run.flatMap {
      case Ok(a) => f(a).run
      case Ko(e) => Status.these[B](e).pure[F]
    })

  def onStatus[B](f: Status[A] => Status[B])(implicit F: Functor[F]): StatusT[F, B] =
    StatusT(run.map(f))

  def mapError(f: These[String, Throwable] => These[String, Throwable])(implicit F: Functor[F]): StatusT[F, A] =
    onStatus(_.mapError(f))

  def isOk(implicit F: Functor[F]): F[Boolean] =
    toOption.map(_.isDefined)

  def isError(implicit F: Functor[F]): F[Boolean] =
    isOk.map(!_)

  def toOption(implicit F: Functor[F]): F[Option[A]] =
    toDisjunction.map(_.toOption)

  def toDisjunction(implicit F: Functor[F]): F[These[String, Throwable] \/ A] =
    run.map(_.toDisjunction)

  def getOrElse[AA >: A](otherwise: =>AA)(implicit F: Functor[F]): F[AA] =
    toOption.map(_.getOrElse(otherwise))

  def |||[AA >: A](otherwise: => StatusT[F, AA])(implicit F: Monad[F]): StatusT[F, AA] =
    StatusT[F, AA](isOk.flatMap(ok => if (ok) this.run else otherwise.run))

  def andFinally(otherwise: =>StatusT[F, Unit])(implicit F: Monad[F]): StatusT[F, A] = {
    StatusT[F, A](run.flatMap { r =>
      try otherwise.run.map(_ => r)
      catch { case t: Throwable => StatusT.exception[F, A](t).run }
    })
  }

}

object StatusT extends LowPriorityStatusT {
  def safe[F[+_]: Monad, A](thunk: => A): StatusT[F, A] =
    StatusT[F, A](Status.safe(thunk).point[F])

  def option[F[+_]: Monad, A](thunk: => A): StatusT[F, Option[A]] =
    StatusT[F, Option[A]](Status.option(thunk).point[F])

  def ok[F[+_]: Monad, A](value: A): StatusT[F, A] =
    StatusT[F, A](Status.ok(value).point[F])

  def status[F[+_]: Monad, A](status: Status[A]): StatusT[F, A] =
    StatusT[F, A](status.point[F])

  def exception[F[+_]: Monad, A](t: Throwable): StatusT[F, A] =
    these[F, A](That(t))

  def fail[F[+_]: Monad, A](message: String): StatusT[F, A] =
    these[F, A](This(message))

  def error[F[+_]: Monad, A](message: String, t: Throwable): StatusT[F, A] =
    these[F, A](Both(message, t))

  def these[F[+_]: Monad, A](both: These[String, Throwable]): StatusT[F, A] =
    StatusT[F, A](Status.these[A](both).point[F])

  def fromDisjunctionF[F[+_]: Functor, A](v: F[These[String, Throwable] \/ A]): StatusT[F, A] =
    StatusT[F, A](v.map(Status.fromDisjunction))

  def fromDisjunction[F[+_]: Monad, A](v: These[String, Throwable] \/ A): StatusT[F, A] =
    fromDisjunctionF(v.point[F])

  def fromIO[A](v: IO[A]): StatusT[IO, A] =
    StatusT(v.map(Status.ok))

  implicit def StatusTMonad[F[+_]: Monad]: Monad[({ type l[+a] = StatusT[F, a] })#l] =
    new Monad[({ type l[a] = StatusT[F, a] })#l] {
      def point[A](v: => A) = ok[F, A](v)
      def bind[A, B](m: StatusT[F, A])(f: A => StatusT[F, B]) = m.flatMap(f)
    }

  implicit def StatusTEqual[F[+_], A](implicit E: Equal[F[Status[A]]]): Equal[StatusT[F, A]] =
    implicitly[Equal[F[Status[A]]]].contramap(_.run)
}

trait LowPriorityStatusT {
  implicit def StatusTMonadIO[F[+_]: MonadIO]: MonadIO[({ type l[a] = StatusT[F, a] })#l] =
    new MonadIO[({ type l[+a] = StatusT[F, a] })#l] {
      def point[A](v: => A) = StatusT.ok[F, A](v)
      def bind[A, B](m: StatusT[F, A])(f: A => StatusT[F, B]) = m.flatMap(f)
      def liftIO[A](ioa: IO[A]): StatusT[F, A] = StatusT(ioa.liftIO[F].map(Status.ok))
    }
}

