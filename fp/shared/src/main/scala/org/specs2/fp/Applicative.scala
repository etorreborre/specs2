package org.specs2.fp

import scala.concurrent.{ExecutionContext, Future}

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Applicative[F[_]] extends Functor[F] { self =>

  def point[A](a: =>A): F[A]

  // alias for point
  final def pure[A](a: =>A): F[A] = point(a)

  def ap[A,B](fa: =>F[A])(f: =>F[A => B]): F[B]

  def ap2[A,B,C](fa: => F[A], fb: => F[B])(f: F[(A,B) => C]): F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap4[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap2(fc, fd)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C, d:D) => f(a,b,c,d)))))
  def ap5[A,B,C,D,E,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap2(fd, fe)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E) => f(a,b,c,d,e)))))
  def ap6[A,B,C,D,E,FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: F[(A,B,C,D,E,FF) => R]): F[R] =
    ap3(fd, fe, ff)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E, ff: FF) => f(a,b,c,d,e,ff)))))
  def ap7[A,B,C,D,E,FF,G,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: F[(A,B,C,D,E,FF,G) => R]): F[R] =
    ap3(fe, ff, fg)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G) => f(a,b,c,d,e,ff,g)))))
  def ap8[A,B,C,D,E,FF,G,H,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: F[(A,B,C,D,E,FF,G,H) => R]): F[R] =
    ap4(fe, ff, fg, fh)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G, h: H) => f(a,b,c,d,e,ff,g,h)))))

  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(point(f))

  def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    apply2(tuple2(fa, fb), fc)((ab, c) => f(ab._1, ab._2, c))
  def apply4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    apply2(tuple2(fa, fb), tuple2(fc, fd))((t, d) => f(t._1, t._2, d._1, d._2))
  def apply5[A, B, C, D, E, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: (A, B, C, D, E) => R): F[R] =
    apply2(tuple3(fa, fb, fc), tuple2(fd, fe))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2))
  def apply6[A, B, C, D, E, FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: (A, B, C, D, E, FF) => R): F[R] =
    apply2(tuple3(fa, fb, fc), tuple3(fd, fe, ff))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3))
  def apply7[A, B, C, D, E, FF, G, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: (A, B, C, D, E, FF, G) => R): F[R] =
    apply2(tuple4(fa, fb, fc, fd), tuple3(fe, ff, fg))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3))
  def apply8[A, B, C, D, E, FF, G, H, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: (A, B, C, D, E, FF, G, H) => R): F[R] =
    apply2(tuple4(fa, fb, fc, fd), tuple4(fe, ff, fg, fh))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4))
  def apply9[A, B, C, D, E, FF, G, H, I, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                            fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I])(f: (A, B, C, D, E, FF, G, H, I) => R): F[R] =
    apply3(tuple3(fa, fb, fc), tuple3(fd, fe, ff), tuple3(fg, fh, fi))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3))
  def apply10[A, B, C, D, E, FF, G, H, I, J, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                                fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                                fi: => F[I], fj: => F[J])(f: (A, B, C, D, E, FF, G, H, I, J) => R): F[R] =
    apply3(tuple3(fa, fb, fc), tuple3(fd, fe, ff), tuple4(fg, fh, fi, fj))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3, t3._4))
  def apply11[A, B, C, D, E, FF, G, H, I, J, K, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                                   fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                                   fi: => F[I], fj: => F[J], fk: => F[K])(f: (A, B, C, D, E, FF, G, H, I, J, K) => R): F[R] =
    apply3(tuple3(fa, fb, fc), tuple4(fd, fe, ff, fg), tuple4(fh, fi, fj, fk))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))
  def apply12[A, B, C, D, E, FF, G, H, I, J, K, L, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                                      fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                                      fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): F[R] =
    apply3(tuple4(fa, fb, fc, fd), tuple4(fe, ff, fg, fh), tuple4(fi, fj, fk, fl))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))

  def tuple2[A,B](fa: => F[A], fb: => F[B]): F[(A,B)] =
    apply2(fa, fb)((_,_))
  def tuple3[A,B,C](fa: => F[A], fb: => F[B], fc: => F[C]): F[(A,B,C)] =
    apply3(fa, fb, fc)((_,_,_))
  def tuple4[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D]): F[(A,B,C,D)] =
    apply4(fa, fb, fc, fd)((_,_,_,_))
  def tuple5[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E]): F[(A,B,C,D,E)] =
    apply5(fa, fb, fc, fd, fe)((_,_,_,_,_))

  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    apply2(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    apply3(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    apply4(_, _, _, _)(f)
  def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (F[A], F[B], F[C], F[D], F[E]) => F[R] =
    apply5(_, _, _, _, _)(f)
  def lift6[A, B, C, D, E, FF, R](f: (A, B, C, D, E, FF) => R): (F[A], F[B], F[C], F[D], F[E], F[FF]) => F[R] =
    apply6(_, _, _, _, _, _)(f)
  def lift7[A, B, C, D, E, FF, G, R](f: (A, B, C, D, E, FF, G) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G]) => F[R] =
    apply7(_, _, _, _, _, _, _)(f)
  def lift8[A, B, C, D, E, FF, G, H, R](f: (A, B, C, D, E, FF, G, H) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H]) => F[R] =
    apply8(_, _, _, _, _, _, _, _)(f)
  def lift9[A, B, C, D, E, FF, G, H, I, R](f: (A, B, C, D, E, FF, G, H, I) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I]) => F[R] =
    apply9(_, _, _, _, _, _, _, _, _)(f)
  def lift10[A, B, C, D, E, FF, G, H, I, J, R](f: (A, B, C, D, E, FF, G, H, I, J) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J]) => F[R] =
    apply10(_, _, _, _, _, _, _, _, _, _)(f)
  def lift11[A, B, C, D, E, FF, G, H, I, J, K, R](f: (A, B, C, D, E, FF, G, H, I, J, K) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K]) => F[R] =
    apply11(_, _, _, _, _, _, _, _, _, _, _)(f)
  def lift12[A, B, C, D, E, FF, G, H, I, J, K, L, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L]) => F[R] =
    apply12(_, _, _, _, _, _, _, _, _, _, _, _)(f)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(point(f))

  // impls of sequence, traverse, etc

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[A, G[_]: Traverse](as: G[F[A]]): F[G[A]] =
    traverse(as)(a => a)

  /** Filter `l` according to an applicative predicate. */
  def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] =
    l match {
      case Nil => point(List())
      case h :: t => ap(filterM(t)(f))(map(f(h))(b => t => if (b) h :: t else t))
    }

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into F.
   */
  def unlessM[A](cond: Boolean)(f: => F[A]): F[Unit] = if (cond) point(()) else void(f)

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into F.
   */
  def whenM[A](cond: Boolean)(f: => F[A]): F[Unit] = if (cond) void(f) else point(())
}

object Applicative {
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  implicit def optionApplicative[L]: Applicative[Option] =
    Monad.optionMonad

  implicit def eitherApplicative[L]: Applicative[Either[L, *]] =
    Monad.eitherMonad[L]

  implicit def futureApplicative(implicit ec: ExecutionContext): Applicative[Future] =
    new Applicative[Future] {
      def point[A](a: =>A): Future[A] =
        Future(a)

      def ap[A,B](fa: =>Future[A])(ff: =>Future[A => B]): Future[B] =
        Future.sequence(List(fa, ff)).map {
          case a :: f :: Nil =>
            f.asInstanceOf[A => B](a.asInstanceOf[A])
          case _ => sys.error("impossible")
        }
    }
}

trait ApplicativeSyntax {

  implicit class ApplicativeOps[F[_] : Applicative, A](fa: F[A]) {
    val applicative = Applicative.apply[F]

    def ap[B](f: F[A => B]): F[B] =
      applicative.ap(fa)(f)

    def tuple2[B](fb: F[B]): F[(A, B)] =
      applicative.tuple2(fa, fb)

    def |@|[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      applicative.apply2(fa, fb)(f)

    def *>[B](fb: F[B]): F[B] =
      applicative.apply2(fa, fb)((_, b) => b)
  }

  implicit class ListApplicativeOps[A](fa: List[A]) {
    def filterM[F[_] : Applicative](f: A => F[Boolean]): F[List[A]] =
      Applicative.apply[F].filterM(fa)(f)
  }

}

