package org.specs2.control.eff

import scalaz._
import scala.annotation.tailrec
import Eff._

/**
 * Effects of type R, returning a value of type A
 *
 * It is implemented as a "Free-er" monad with extensible effects:
 *
 *  - the "pure" case is a pure value of type A
 *
 *  - the "impure" case is:
 *     - a disjoint union of possible effects
 *     - a continuation of type X => Eff[R, A] indicating what to do if the current effect is of type M[X]
 *       this type is represented by the `Arrs` type
 *
 * The monad implementation for this type is really simple:
 *
 *  - `point` is Pure
 *  - `bind` simply appends the binding function to the `Arrs` continuation
 *
 * Important:
 *
 *  The list of continuations is NOT implemented as a type sequence but simply as a
 *    Vector[Any => Eff[R, Any]]
 *
 *  This means that various `.asInstanceOf` are present in the implementation and could lead
 *  to burns and severe harm. Use with caution!
 *
 * @see http://okmij.org/ftp/Haskell/extensible/more.pdf
 *
 */
sealed trait Eff[R, A] {
  def map[B](f: A => B): Eff[R, B] =
    EffMonad[R].map(this)(f)

  def ap[B](f: Eff[R, A => B]): Eff[R, B] =
    EffApplicative[R].ap(this)(f)

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    EffMonad[R].bind(this)(f)

  def flatten[B](implicit ev: A =:= Eff[R, B]): Eff[R, B] =
    EffMonad[R].bind(this)(a => a)

}

case class Pure[R, A](value: A) extends Eff[R, A]

/**
 * union is a disjoint union of effects returning a value of type X (not specified)
 */
case class Impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A]) extends Eff[R, A]

/**
 * union is a disjoint union of effects returning a value of type X (not specified)
 */
case class ImpureAp[R, X, A](union: Union[R, X], continuation: Apps[R, X, A]) extends Eff[R, A] {
  def toMonadic: Eff[R, A] =
    Impure[R, union.X, A](union, Arrs.singleton((x: union.X) => continuation(x)))
}

object Eff extends EffCreation with
  EffInterpretation with
  EffImplicits

trait EffImplicits {

  /**
   * Monad implementation for the Eff[R, ?] type
   */
  implicit def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Pure(a)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(a) =>
          f(a)

        case Impure(union, continuation) =>
          Impure(union, continuation.append(f))

        case ap @ ImpureAp(_, _) =>
          ap.toMonadic.flatMap(f)
      }
  }

  def EffApplicative[R]: Applicative[Eff[R, ?]] = new Applicative[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Pure(a)

    def ap[A, B](fa: =>Eff[R, A])(ff: =>Eff[R, A => B]): Eff[R, B] =
      fa match {
        case Pure(a) =>
          ff.map(f => f(a))

        case Impure(union, continuation) =>
          fa.flatMap(a => ff.map(f => f(a)))

        case ImpureAp(union, continuation) =>
          ImpureAp(union, continuation.append(ff))

      }
  }

}

object EffImplicits extends EffImplicits

trait EffCreation {
  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: T |= R): Eff[R, V] =
    impure(member.inject(tv), Arrs.unit)

  /** use the internal effect as one of the stack effects */
  def collapse[R, M[_], A](r: Eff[R, M[A]])(implicit m: M |= R): Eff[R, A] =
    EffMonad[R].bind(r)(mx => send(mx)(m))

  /** create an Eff value for () */
  def unit[R]: Eff[R, Unit] =
    EffMonad.point(())

  /** create a pure value */
  def pure[R, A](a: A): Eff[R, A] =
    Pure(a)

  /** create a impure value from an union of effects and a continuation */
  def impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A]): Eff[R, A] =
    Impure[R, X, A](union, continuation)

  /** apply a function to an Eff value using the applicative instance */
  def ap[R, A, B](a: =>Eff[R, A])(f: =>Eff[R, A => B]): Eff[R, B] =
    EffImplicits.EffApplicative[R].ap(a)(f)

  /** use the applicative instance of Eff to traverse a list of values */
  def traverseA[R, F[_] : Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to sequenc a list of values */
  def sequenceA[R, F[_] : Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(EffImplicits.EffApplicative[R])
}

object EffCreation extends EffCreation

trait EffInterpretation {
  /**
   * base runner for an Eff value having no effects at all
   *
   * This runner can only return the value in Pure because it doesn't
   * known how to interpret the effects in Impure
   */
  def run[A](eff: Eff[NoFx, A]): A =
    eff match {
      case Pure(a) => a
      case other   => sys.error("impossible: cannot run the effects in "+other)
    }

  /**
   * peel-off the only present effect
   */
  def detach[M[_] : Monad, A](eff: Eff[Fx1[M], A]): M[A] = {
    def go(e: Eff[Fx1[M], A]): M[A] = {
      e match {
        case Pure(a) => Monad[M].point(a)

        case Impure(u, continuation) =>
          u match {
            case Union1(ta) => Monad[M].bind(ta)(x => go(continuation(x)))
          }

        case ImpureAp(u, continuation) =>
          u match {
            case Union1(ta) => Monad[M].bind(ta)(x => go(continuation(x)))
          }
      }
    }
    go(eff)
  }

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a) => Option(a)
      case _ => None
    }

  /**
   * An Eff[R, A] value can be transformed into an Eff[U, A]
   * value provided that all the effects in R are also in U
   */
  def effInto[R, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U]): Eff[U, A] =
    f(e)
}

object EffInterpretation extends EffInterpretation

/**
 * Sequence of monadic functions from A to B: A => Eff[B]
 *
 * Internally it is represented as a Vector of functions:
 *
 *  A => Eff[R, X1]; X1 => Eff[R, X2]; X2 => Eff[R, X3]; ...; X3 => Eff[R, B]
 *
 */
case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) {

  /**
   * append a new monadic function to this list of functions such that
   *
   * Arrs[R, A, B] => (B => Eff[R, C]) => Arrs[R, A, C]
   *
   */
  def append[C](f: B => Eff[R, C]): Arrs[R, A, C] =
    Arrs(functions :+ f.asInstanceOf[Any => Eff[R, Any]])

  /** map the last returned effect */
  def mapLast(f: Eff[R, B] => Eff[R, B]): Arrs[R, A, B] =
    functions match {
      case Vector() => this
      case fs :+ last => Arrs(fs :+ ((x: Any) => f(last(x).asInstanceOf[Eff[R, B]]).asInstanceOf[Eff[R, Any]]))
    }

  /**
   * execute this monadic function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any): Eff[R, B] = {
      fs match {
        case Vector() =>
          Eff.EffMonad[R].point(v).asInstanceOf[Eff[R, B]]

        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]]

        case f +: rest =>
          f(v) match {
            case Pure(a1) =>
              go(rest, a1)

            case Impure(u, q) =>
              Impure[R, u.X, B](u, q.copy(functions = q.functions ++ rest))

            case ap @ ImpureAp(u, continuation) =>
              val monadicArrs = Arrs.singleton((x: u.X) => continuation(x))
              Impure[R, u.X, B](u, monadicArrs.copy(functions = monadicArrs.functions ++ rest))
          }
      }
    }

    go(functions, a)
  }

  def contramap[C](f: C => A): Arrs[R, C, B] =
    Arrs(((c: Any) => Eff.EffMonad[R].point(f(c.asInstanceOf[C]).asInstanceOf[Any])) +: functions)

  def transform[U, M[_], N[_]](t: NaturalTransformation[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, R, U]): Arrs[R, A, B] =
    Arrs(functions.map(f => (x: Any) => Interpret.transform(f(x), t)(m, n)))
}

object Arrs {

  /** create an Arrs function from a single monadic function */
  def singleton[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] =
    Arrs(Vector(f.asInstanceOf[Any => Eff[R, Any]]))

  /** create an Arrs function with no effect, which is similar to using an identity a => EffMonad[R].point(a) */
  def unit[R, A]: Arrs[R, A, A] =
    Arrs(Vector())
}

/**
 * Sequence of applicative functions from A to B: Eff[R, A => B]
 *
 */
case class Apps[R, A, B](functions: Vector[Eff[R, Any => Any]]) {

  def append[C](f: Eff[R, B => C]): Apps[R, A, C] =
    Apps(functions :+ f.asInstanceOf[Eff[R, Any => Any]])

  /**
   * execute this data structure as function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Eff[R, Any => Any]], v: Eff[R, Any]): Eff[R, B] = {
      fs match {
        case Vector() =>
          v.asInstanceOf[Eff[R, B]]

        case Vector(ff) =>
          ff.flatMap(f => v.map(f)).asInstanceOf[Eff[R, B]]

        case ff +: rest =>
          go(rest, ff.flatMap(f => v.map(f)))
      }
    }

    go(functions, Eff.EffMonad[R].point(a).asInstanceOf[Eff[R, Any]])
  }
}

object Apps {

  /** create an Apps function from a single applicative function */
  def singleton[R, A, B](f: Eff[R, A => B]): Apps[R, A, B] =
    Apps(Vector(f.asInstanceOf[Eff[R, Any => Any]]))

  /** create an Apps function with no effect */
  def unit[R, A]: Apps[R, A, A] =
    Apps(Vector())
}
