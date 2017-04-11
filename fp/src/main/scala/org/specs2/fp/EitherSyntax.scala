package org.specs2.fp

import scala.util.{Failure, Success, Try}

/**
 * Inspired from the cats (https://github.com/typelevel/cats project
 */
trait EitherSyntax {
  implicit def syntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)

  implicit def syntaxEitherObject(either: Either.type): EitherObjectOps = new EitherObjectOps(either)

  implicit def syntaxLeft[A, B](left: Left[A, B]): LeftOps[A, B] = new LeftOps(left)

  implicit def syntaxRight[A, B](right: Right[A, B]): RightOps[A, B] = new RightOps(right)

  implicit def syntaxEitherId[A](a: A): EitherIdOps[A] = new EitherIdOps(a)
}

final class EitherOps[A, B](val eab: Either[A, B]) extends AnyVal {
  def foreach(f: B => Unit): Unit = eab match {
    case Left(_)  => ()
    case Right(b) => f(b)
  }

  def getOrElse[BB >: B](default: => BB): BB = eab match {
    case Left(_)  => default
    case Right(b) => b
  }

  def orElse[C, BB >: B](fallback: => Either[C, BB]): Either[C, BB] = eab match {
    case Left(_)      => fallback
    case r @ Right(_) => EitherUtil.leftCast(r)
  }

  def recover[BB >: B](pf: PartialFunction[A, BB]): Either[A, BB] = eab match {
    case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
    case _                            => eab
  }

  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, Either[AA, BB]]): Either[AA, BB] = eab match {
    case Left(a) if pf.isDefinedAt(a) => pf(a)
    case _                            => eab
  }

  def valueOr[BB >: B](f: A => BB): BB = eab match {
    case Left(a)  => f(a)
    case Right(b) => b
  }

  def forall(f: B => Boolean): Boolean = eab match {
    case Left(_)  => true
    case Right(b) => f(b)
  }

  def exists(f: B => Boolean): Boolean = eab match {
    case Left(_)  => false
    case Right(b) => f(b)
  }

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): Either[AA, B] = eab match {
    case Left(_)  => eab
    case Right(b) => if (f(b)) eab else Left(onFailure)
  }

  def toOption: Option[B] = eab match {
    case Left(_)  => None
    case Right(b) => Some(b)
  }

  def toList: List[B] = eab match {
    case Left(_)  => Nil
    case Right(b) => List(b)
  }

  def toTry(implicit ev: A <:< Throwable): Try[B] = eab match {
    case Left(a)  => Failure(ev(a))
    case Right(b) => Success(b)
  }

  def bimap[C, D](fa: A => C, fb: B => D): Either[C, D] = eab match {
    case Left(a)  => Left(fa(a))
    case Right(b) => Right(fb(b))
  }

  def map[C](f: B => C): Either[A, C] = eab match {
    case l @ Left(_) => EitherUtil.rightCast(l)
    case Right(b)    => Right(f(b))
  }

  def leftMap[C](f: A => C): Either[C, B] = eab match {
    case Left(a)      => Left(f(a))
    case r @ Right(_) => EitherUtil.leftCast(r)
  }

  def flatMap[AA >: A, D](f: B => Either[AA, D]): Either[AA, D] = eab match {
    case l @ Left(_) => EitherUtil.rightCast(l)
    case Right(b)    => f(b)
  }

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[Either[AA, D]] = eab match {
    case l @ Left(_) => F.pure(EitherUtil.rightCast(l))
    case Right(b)    => F.map(f(b))(Right(_))
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = eab match {
    case Left(_)  => c
    case Right(b) => f(c, b)
  }

  final def append[AA >: A, BB >: B](that: Either[AA, BB])(implicit BB: Semigroup[BB]): Either[AA, BB] = eab match {
    case left @ Left(_) => left
    case Right(b1) => that match {
      case left @ Left(_) => left
      case Right(b2) => Right(BB.append(b1, b2))
    }
  }

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = eab match {
    case Left(a)  => s"Left(${AA.show(a)})"
    case Right(b) => s"Right(${BB.show(b)})"
  }

  def ap[AA >: A, BB >: B, C](that: Either[AA, BB => C]): Either[AA, C] = new EitherOps(that).flatMap(this.map)

}

final class EitherObjectOps(val either: Either.type) extends AnyVal { // scalastyle:off ensure.single.space.after.token
  def left[A, B](a: A): Either[A, B] = Left(a)

  def right[A, B](b: B): Either[A, B] = Right(b)

  def catchNonFatal[A](f: => A): Either[Throwable, A] =
    try {
      right(f)
    } catch {
      case scala.util.control.NonFatal(t) => left(t)
    }

  def fromTry[A](t: Try[A]): Either[Throwable, A] =
    t match {
      case Failure(e) => left(e)
      case Success(v) => right(v)
    }

  def fromOption[A, B](o: Option[B], ifNone: => A): Either[A, B] = o match {
    case None    => left[A, B](ifNone)
    case Some(a) => right(a)
  }
}

final class LeftOps[A, B](val left: Left[A, B]) extends AnyVal {
  def rightCast[C]: Either[A, C] = left.asInstanceOf[Either[A, C]]
}

final class RightOps[A, B](val right: Right[A, B]) extends AnyVal {
  def leftCast[C]: Either[C, B] = right.asInstanceOf[Either[C, B]]
}

final class EitherIdOps[A](val obj: A) extends AnyVal {
  def asLeft[B]: Either[A, B] = Left(obj)
  def asRight[B]: Either[B, A] = Right(obj)
}

private[fp] object EitherUtil {
  def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
    right.asInstanceOf[Either[C, B]]

  def rightCast[A, B, C](left: Left[A, B]): Either[A, C] =
    left.asInstanceOf[Either[A, C]]
}
