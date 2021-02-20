package org.specs2.fp

import scala.concurrent.{ExecutionContext, Future}

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Monad[F[_]] extends Applicative[F]:

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    bind(fa)(f)

  def join[A](ffa: F[F[A]]): F[A] =
    bind(ffa)(a => a)

  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] =
    bind(fa)(a => map(f)(f1 => f1(a)))

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    bind(fa)(a => point(f(a)))

  def tailrecM[A, B](a: A)(f: A => F[A Either B]): F[B] =
    bind(f(a)) {
      case Left(a1) => tailrecM(a1)(f)
      case Right(b) => point(b)
    }


  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if p(y) then iterateWhile(f)(p) else point(y))

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if p(y) then point(y) else iterateUntil(f)(p))

object Monad:

  given idMonad: Monad[Id] with
    def point[A](a: =>A): Id[A] = a

    def bind[A,B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa)

  given optionMonad: Monad[Option] with
    def point[A](a: =>A): Option[A] = Some(a)

    def bind[A,B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match
        case None => None
        case Some(a) => f(a)

    override def tailrecM[A, B](a: A)(f: A => Option[A Either B]): Option[B] =
      f(a) match
        case None => None
        case Some(Left(a1)) => tailrecM(a1)(f)
        case Some(Right(b)) => Some(b)

  given eitherMonad[L]: Monad[Either[L, *]] with
    def point[A](a: =>A): Either[L, A] = Right(a)

    def bind[A,B](fa: Either[L, A])(f: A => Either[L, B]): Either[L, B] =
      fa match
        case Left(l) => Left(l)
        case Right(a) => f(a)

    override def tailrecM[A, B](a: A)(f: A => Either[L, A Either B]): Either[L, B] =
      f(a) match
        case Left(l) => Left(l)
        case Right(Left(a1)) => tailrecM(a1)(f)
        case Right(Right(b)) => Right(b)

  given futureMonad(using ec: ExecutionContext): Monad[Future] with
    def point[A](a: =>A): Future[A] = Future.successful(a)

    def bind[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)

trait MonadSyntax:

  implicit class MonadOps[F[_] : Monad, A, B](fa: F[A]):

    def flatMap(f: A => F[B]): F[B] =
      summon[Monad[F]].flatMap(fa)(f)

    def bind(f: A => F[B]): F[B] =
      summon[Monad[F]].bind(fa)(f)

    def >>=(f: A => F[B]): F[B] =
      summon[Monad[F]].bind(fa)(f)

    def >>(fb: F[B]): F[B] =
      summon[Monad[F]].bind(fa)(_ => fb)

  implicit class MonadFlattenOps[F[_] : Monad, A](fa: F[F[A]]):

    def flatten: F[A] =
      summon[Monad[F]].flatMap(fa)(identity)
