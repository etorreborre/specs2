package org.specs2.fp

import scala.util.{Failure, Success, Try}

/** Inspired from the cats (https://github.com/typelevel/cats project
  */
trait EitherSyntax:

  extension [A, B](eab: Either[A, B])

    def foreach(f: B => Unit): Unit = eab match
      case Left(_)  => ()
      case Right(b) => f(b)

    def forall(f: B => Boolean): Boolean = eab match
      case Left(_)  => true
      case Right(b) => f(b)

    def exists(f: B => Boolean): Boolean = eab match
      case Left(_)  => false
      case Right(b) => f(b)

    def toOption: Option[B] = eab match
      case Left(_)  => None
      case Right(b) => Some(b)

    def toList: List[B] = eab match
      case Left(_)  => Nil
      case Right(b) => List(b)

  extension [A, B, AA >: A, BB >: B, C](eab: Either[A, B])
    def getOrElse(default: =>BB): BB = eab match
      case Left(_)  => default
      case Right(b) => b

    def orElse(fallback: =>Either[C, BB]): Either[C, BB] = eab match
      case Left(_)      => fallback
      case r @ Right(_) => EitherUtil.leftCast(r)

    def recover(pf: PartialFunction[A, BB]): Either[A, BB] = eab match
      case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
      case _                            => eab

    def recoverWith(pf: PartialFunction[A, Either[AA, BB]]): Either[AA, BB] = eab match
      case Left(a) if pf.isDefinedAt(a) => pf(a)
      case _                            => eab

    def valueOr(f: A => BB): BB = eab match
      case Left(a)  => f(a)
      case Right(b) => b

    def ensure(onFailure: =>AA)(f: B => Boolean): Either[AA, B] = eab match
      case Left(_)  => eab
      case Right(b) => if f(b) then eab else Left(onFailure)

    def toTry(using ev: A <:< Throwable): Try[B] = eab match
      case Left(a)  => Failure(ev(a))
      case Right(b) => Success(b)

    def map(f: B => C): Either[A, C] = eab match
      case l @ Left(_) => EitherUtil.rightCast(l)
      case Right(b)    => Right(f(b))

    def as(c: =>C): Either[A, C] =
      eab.map(_ => c)

    def leftMap(f: A => C): Either[C, B] = eab match
      case Left(a)      => Left(f(a))
      case r @ Right(_) => EitherUtil.leftCast(r)

    def append(that: Either[AA, BB])(using BB: Semigroup[BB]): Either[AA, BB] = eab match
      case left @ Left(_) => left
      case Right(b1)      =>
        that match
          case left @ Left(_) => left
          case Right(b2)      => Right(BB.append(b1, b2))

    def show(using AA: Show[AA], BB: Show[BB]): String = eab match
      case Left(a)  => s"Left(${AA.show(a)})"
      case Right(b) => s"Right(${BB.show(b)})"

    def ap(that: Either[AA, BB => C]): Either[AA, C] =
      that.flatMap(eab.map)

    def flatMap(f: B => Either[AA, C]): Either[AA, C] = eab match
      case l @ Left(_) => EitherUtil.rightCast(l)
      case Right(b)    => f(b)

    def foldLeft(c: C)(f: (C, B) => C): C = eab match
      case Left(_)  => c
      case Right(b) => f(c, b)

  extension [A, B, C, D](eab: Either[A, B])
    def bimap(fa: A => C, fb: B => D): Either[C, D] = eab match
      case Left(a)  => Left(fa(a))
      case Right(b) => Right(fb(b))

  extension [F[_], A, B, AA >: A, C](eab: Either[A, B])
    def traverse(f: B => F[C])(using F: Applicative[F]): F[Either[AA, C]] = eab match
      case l @ Left(_) => F.pure(EitherUtil.rightCast(l))
      case Right(b)    => F.map(f(b))(Right(_))

private[fp] object EitherUtil:

  def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
    right.asInstanceOf[Either[C, B]]

  def rightCast[A, B, C](left: Left[A, B]): Either[A, C] =
    left.asInstanceOf[Either[A, C]]

  def catchNonFatal[A](f: =>A): Either[Throwable, A] =
    try Right(f)
    catch case scala.util.control.NonFatal(t) => Left(t)

  def fromTry[A](t: Try[A]): Either[Throwable, A] =
    t match
      case Failure(e) => Left(e)
      case Success(v) => Right(v)

  def fromOption[A, B](o: Option[B], ifNone: =>A): Either[A, B] = o match
    case Some(a) => Right(a)
    case _       => Left[A, B](ifNone)
