package org.specs2.fp

import scala.concurrent.{ExecutionContext, Future}

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Monad[F[_]] extends Applicative[F] {

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    bind(fa)(f)

  def join[A](ffa: F[F[A]]): F[A] =
    bind(ffa)(a => a)

  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] =
    bind(fa)(a => map(f)(f1 => f1(a)))

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    bind(fa)(a => point(f(a)))

  def tailrecM[A, B](f: A => F[A Either B])(a: A): F[B] =
    bind(f(a)) {
      case Left(a1) => tailrecM(f)(a1)
      case Right(b) => point(b)
    }


  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if (p(y)) iterateWhile(f)(p) else point(y))

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if (p(y)) point(y) else iterateUntil(f)(p))
}

object Monad {

  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def point[A](a: =>A): Id[A] = a

    def bind[A,B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def point[A](a: =>A): Option[A] = Some(a)

    def bind[A,B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    override def tailrecM[A, B](f: A => Option[A Either B])(a: A): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(a1)) => tailrecM(f)(a1)
        case Some(Right(b)) => Some(b)
      }
  }

  implicit def eitherMonad[L]: Monad[Either[L, *]] = new Monad[Either[L, *]] {
    def point[A](a: =>A): Either[L, A] = Right(a)

    def bind[A,B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] =
      fa match {
        case Left(l) => Left(l)
        case Right(a) => f(a)
      }

    override def tailrecM[A, B](f: A => Either[L, A Either B])(a: A): Either[L, B] =
      f(a) match {
        case Left(l) => Left(l)
        case Right(Left(a1)) => tailrecM(f)(a1)
        case Right(Right(b)) => Right(b)
      }
  }

  implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    def point[A](a: =>A): Future[A] = Future.successful(a)

    def bind[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)
  }
}

trait MonadSyntax {

  implicit class MonadOps[F[_] : Monad, A](fa: F[A]) {
    val monad = Monad.apply[F]

    def flatMap[B](f: A => F[B]): F[B] =
      monad.flatMap(fa)(f)

    def bind[B](f: A => F[B]): F[B] =
      monad.bind(fa)(f)

    def >>=[B](f: A => F[B]): F[B] =
      bind(f)

    def >>[B](fb: F[B]): F[B] =
      bind(_ => fb)
  }

}
