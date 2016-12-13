package org.specs2.control.eff

import scalaz._, Scalaz._

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
 *  - the "impure applicative" case is:
 *     - list of disjoint unions of possible effects
 *     - a function to apply to the values resulting from those effects
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
 *  Similarly the list of effects in the applicative case is untyped and interpreters for those effects
 *  are supposed to create a list of values to feed the mapping function. If an interpreter doesn't
 *  create a list of values of the right size and with the right types, there will be a runtime exception.
 *
 * The Pure, Impure and ImpureAp cases also incorporate a "last" action returning no value but just used
 * for side-effects (shutting down an execution context for example). This action is meant to be executed at the end
 * of all computations, regardless of the number of flatMaps added on the Eff value.
 *
 * Since this last action will be executed, its value never collected so if it throws an exception it is possible
 * to print it by defining the eff.debuglast system property (-Deff.debuglast=true)
 *
 * @see http://okmij.org/ftp/Haskell/extensible/more.pdf
 *
 */
sealed trait Eff[R, A] {

  def map[B](f: A => B): Eff[R, B] =
    EffApplicative[R].map(this)(f)

  def ap[B](f: Eff[R, A => B]): Eff[R, B] =
    EffApplicative[R].ap(this)(f)

  def *>[B](fb: Eff[R, B]): Eff[R, B] =
    EffApplicative.tuple2(this, fb).map { case (a, b) => b }

  def <*[B](fb: Eff[R, B]): Eff[R, A] =
    EffApplicative.tuple2(this, fb).map { case (a, b) => a }

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    EffMonad[R].bind(this)(f)

  def flatten[B](implicit ev: A =:= Eff[R, B]): Eff[R, B] =
    flatMap(a => a)

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: =>Eff[R, Unit]): Eff[R, A] =
    flatMap(a => pure(a).addLast(Last.eff(l)))

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: Last[R]): Eff[R, A]

}

case class Pure[R, A](value: A, last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Pure(value, last <* l)
}

/**
 * Impure is an effect (encoded as one possibility among other effects, a Union)
 * and a continuation providing the next Eff value.
 *
 * This essentially models a flatMap operation with the current effect
 * and the monadic function to apply to a value once the effect is interpreted
 *
 * One effect can always be executed last, just for side-effects
 */
case class Impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Impure[R, X, A](union, continuation, last <* l)
}

/**
 * ImpureAp is a list of independent effects and a pure function
 * creating a value with all the resulting values once all effects have
 * been interpreted.
 *
 * This essentially models a sequence + map operation but it is important to understand that the list of
 * Union objects can represent different effects and be like: List[Option[Int], Future[String], Option[Int]].
 *
 * Interpreting such an Eff value for a given effect (say Option) consists in:
 *
 *  - grouping all the Option values,
 *  - sequencing them
 *  - pass them to a continuation which will apply the 'map' functions when the other effects (Future in the example
 *  above) will have been interpreted
 *
 * VERY IMPORTANT:
 *
 *  - this object is highly unsafe
 *  - the size of the list argument to 'map' must always be equal to the number of unions in the Unions object
 *  - the types of the elements in the list argument to 'map' must be the exact types of each effect in unions.unions
 *
 */
case class ImpureAp[R, X, A](unions: Unions[R, X], continuation: Arrs[R, List[Any], A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def toMonadic: Eff[R, A] =
    Impure[R, unions.X, A](unions.first, unions.continueWith(continuation), last)

  def addLast(l: Last[R]): Eff[R, A] =
    ImpureAp[R, X, A](unions, continuation, last <* l)
}

object Eff extends EffCreation with
  EffInterpretation with
  EffImplicits

trait EffImplicits {

  /**
   * Monad implementation for the Eff[R, ?] type
   */
  implicit final def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] with BindRec[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Pure(a)

    override def map[A, B](fa: Eff[R, A])(f: A => B): Eff[R, B] =
      fa match {
        case Pure(a, l) =>
          pure(f(a)).addLast(l)

        case Impure(union, continuation, last) =>
          Impure(union, continuation map f, last)

        case ImpureAp(unions, continuations, last) =>
          ImpureAp(unions, continuations map f, last)
      }

    /**
     * When flatMapping the last action must still be executed after the next action
     */
    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(a, l) =>
          f(a).addLast(l)
      
        case Impure(union, continuation, last) =>
          Impure(union, continuation.append(f), last)
      
        case ImpureAp(unions, continuation, last) =>
          ImpureAp(unions, continuation.append(f), last)
      }

    def tailrecM[A, B](f: A => Eff[R, \/[A, B]])(a: A): Eff[R, B] =
      bind(f(a)) {
        case \/-(b)   => pure(b)
        case -\/(next) => tailrecM(f)(next)
      }

  }

  def EffApplicative[R]: Applicative[Eff[R, ?]] = new Applicative[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Pure(a)

    def ap[A, B](fa: =>Eff[R, A])(ff: =>Eff[R, A => B]): Eff[R, B] =
      fa match {
        case Pure(a, last) =>
          ff match {
            case Pure(f, last1)        => Pure(f(a), last1).addLast(last)
            case Impure(u, c, last1)   => ImpureAp(Unions(u, Nil), Arrs.singleton(ls => c(ls.head).map(_(a))), last1 *> last)
            case ImpureAp(u, c, last1) => ImpureAp(u, Arrs.singleton(xs => c(xs).map(_(a))), last1 *> last)
          }

        case Impure(u, c, last) =>
          ff match {
            case Pure(f, last1)          => ImpureAp(Unions(u, Nil), Arrs.singleton(ls => c(ls.head).map(f)), last1 *> last)
            case Impure(u1, c1, last1)   => ImpureAp(Unions(u, List(u1)),  Arrs.singleton(ls => ap(c(ls.head))(c1(ls(1)))), last1 *> last)
            case ImpureAp(u1, c1, last1) => ImpureAp(Unions(u, u1.unions), Arrs.singleton(ls => ap(c(ls.head))(c1(ls.drop(1)))), last1 *> last)
          }

        case ImpureAp(unions, c, last) =>
          ff match {
            case Pure(f, last1)         => ImpureAp(unions, c map f, last1 *> last)
            case Impure(u, c1, last1)   => ImpureAp(Unions(unions.first, unions.rest :+ u), Arrs.singleton(ls => ap(c(ls.dropRight(1)))(c1(ls.last))), last1 *> last)
            case ImpureAp(u, c1, last1) => ImpureAp(u append unions, Arrs.singleton(xs => ap(c(xs.drop(u.size)))(c1(xs.take(u.size)))), last1 *> last)
          }

      }
  }

}

object EffImplicits extends EffImplicits

trait EffCreation {
  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: T |= R): Eff[R, V] =
    ImpureAp(Unions(member.inject(tv), Nil), Arrs.singleton(xs => pure[R, V](xs.head.asInstanceOf[V])))

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
  def ap[R, A, B](a: Eff[R, A])(f: Eff[R, A => B]): Eff[R, B] =
    EffImplicits.EffApplicative[R].ap(a)(f)

  /** use the applicative instance of Eff to traverse a list of values */
  def traverseA[R, F[_] : Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to sequence a list of values */
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
      case Pure(a, Last(Some(l))) => l.value; a
      case Pure(a, Last(None))    => a
      case other                  => sys.error("impossible: cannot run the effects in "+other)
    }

  /**
   * peel-off the only present effect
   */
  def detach[M[_] : Monad, A](eff: Eff[Fx1[M], A])(implicit bindRec: BindRec[M]): M[A] =
    bindRec.tailrecM[Eff[Fx1[M], A], A] {
      case Pure(a, Last(Some(l))) => Monad[M].pure(-\/(l.value.as(a)))
      case Pure(a, Last(None))    => Monad[M].pure(\/-(a))
    
      case Impure(u, continuation, last) =>
        u match {
          case Union1(ta) =>
            last match {
              case Last(Some(l)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
              case Last(None)    => Monad[M].map(ta)(x => -\/(continuation(x)))
            }
        }
    
      case ap @ ImpureAp(u, continuation, last) =>
        Monad[M].point(-\/(ap.toMonadic))
    }(eff)

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M], bindRec: BindRec[M], applicative: Applicative[M]): M[A] =
    bindRec.tailrecM[Eff[Fx1[M], A], A] {
      case Pure(a, Last(Some(l))) => monad.pure(-\/(l.value.as(a)))

      case Pure(a, Last(None))    => monad.pure(\/-(a))
    
      case Impure(u, continuation, last) =>

        u match {
          case Union1(ta) =>
            last match {
              case Last(Some(l)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
              case Last(None)    => Monad[M].map(ta)(x => -\/(continuation(x)))
            }
        }
    
      case ap @ ImpureAp(unions, continuation, last) =>
        val effects = unions.unions.collect { case Union1(mx) => mx }
        val sequenced = applicative.sequence(effects)

        last match {
          case Last(Some(l)) => Monad[M].map(sequenced)(x => -\/(continuation(x).addLast(last)))
          case Last(None)    => Monad[M].map(sequenced)(x => -\/(continuation(x)))
        }
    }(eff)

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a, Last(Some(l))) => l.value; Option(a)
      case Pure(a, _)             => Option(a)
      case _                      => None
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
case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) extends (A => Eff[R, B]) {

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

  /** map the last value */
  def map[C](f: B => C): Arrs[R, A, C] =
    Arrs(functions :+ ((x: Any) => pure[R, Any](f(x.asInstanceOf[B]).asInstanceOf[Any])))

  /**
   * execute this monadic function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any, last: Last[R] = Last.none[R]): Eff[R, B] = {
      fs match {
        case Vector() =>
          Pure[R, B](v.asInstanceOf[B], last)

        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]].addLast(last)

        case f +: rest =>
          f(v) match {
            case Pure(a1, l1) =>
              go(rest, a1, last *> l1)

            case Impure(u, q, l) =>
              Impure[R, u.X, B](u, q.copy(q.functions ++ rest), last *> l)

            case ap @ ImpureAp(unions, q, l) =>
              ImpureAp[R, unions.X, B](unions, q.copy(q.functions ++ rest), last *> l)
          }
      }
    }

    go(functions, a)
  }

  def contramap[C](f: C => A): Arrs[R, C, B] =
    Arrs(((c: Any) => Eff.EffMonad[R].point(f(c.asInstanceOf[C]).asInstanceOf[Any])) +: functions)

  def transform[U, M[_], N[_]](t: ~>[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, R, U]): Arrs[R, A, B] =
    Arrs(functions.map(f => (x: Any) => Interpret.transform(f(x), t)(m, n)))
}

object Arrs {

  /** create an Arrs function from a single monadic function */
  def singleton[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] =
  Arrs(Vector(f.asInstanceOf[Any => Eff[R, Any]]))

  /** create an Arrs function with no effect, which is similar to using an identity a => EffMonad[R].pure(a) */
  def unit[R, A]: Arrs[R, A, A] =
  Arrs(Vector())
}

