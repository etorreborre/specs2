package org.specs2
package control
package eff

import fp._
import fp.syntax._
import Eff._

import scala.concurrent.duration.FiniteDuration

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
    EffApplicative[R].apply2(this, fb) { case (_, b) => b }

  def <*[B](fb: Eff[R, B]): Eff[R, A] =
    EffApplicative[R].apply2(this, fb) { case (a, _) => a }

  def >>=[B](f: A => Eff[R, B]): Eff[R, B] =
    flatMap(f)

  def >>[B](fb: Eff[R, B]): Eff[R, B] =
    flatMap(_ => fb)

  def <<[B](fb: Eff[R, B]): Eff[R, A] =
    flatMap(a => fb.map(_ => a))

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    EffMonad[R].flatMap(this)(f)

  def flatten[B](implicit ev: A <:< Eff[R, B]): Eff[R, B] =
    flatMap(ev(_))

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: =>Eff[R, Unit]): Eff[R, A] =
    addLast(Last.eff(l))

  /** add one last action to be executed after any computation chained to this Eff value */
  private[eff] def addLast(l: Last[R]): Eff[R, A]

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
case class Impure[R, X, A](union: Effect[R, X], continuation: Continuation[R, X, A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Impure[R, X, A](union, continuation, last <* l)
}

/**
 * ImpureAp is a list of independent effects and a pure function
 * creating a value with all the resulting values once all effects have
 * been interpreted.
 *
 * This essentially models a sequence + map operation but it is important to understand that the list of
 * Union objects can represent different effects and be like: Vector[Option[Int], Future[String], Option[Int]].
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
case class ImpureAp[R, X, A](unions: Unions[R, X], continuation: Continuation[R, Vector[Any], A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
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
  final val effMonadUnsafe: Monad[Eff[AnyRef, ?]] = new Monad[Eff[AnyRef, ?]] {

    def point[A](a: =>A): Eff[AnyRef, A] =
      Pure[AnyRef, A](a)

    override def map[A, B](fa: Eff[AnyRef, A])(f: A => B): Eff[AnyRef, B] =
      fa match {
        case Pure(a, l) =>
          Impure(NoEffect(a), Continuation.unit.map(f), l)

        case Impure(union, continuation, last) =>
          Impure(union, continuation map f, last)

        case ImpureAp(unions, continuations, last) =>
          ImpureAp(unions, continuations map f, last)
      }

    /**
     * When flatMapping the last action must still be executed after the next action
     */
    def bind[A, B](fa: Eff[AnyRef, A])(f: A => Eff[AnyRef, B]): Eff[AnyRef, B] =
      fa match {
        case Pure(a, l) =>
          Impure[AnyRef, A, B](NoEffect[AnyRef, A](a), Continuation.lift(x => f(x).addLast(l)))

        case Impure(union, continuation, last) =>
          Impure(union, continuation.append(f), last)

        case ImpureAp(unions, continuation, last) =>
          ImpureAp(unions, continuation.append(f), last)
      }

    def tailRecM[A, B](a: A)(f: A => Eff[AnyRef, Either[A, B]]): Eff[AnyRef, B] =
      f(a) match {
        case Pure(v, l) => v match {
          case Left(a1) => tailRecM(a1)(f)
          case Right(b) => Pure(b)
        }
        case Impure(u, c, l) =>
          Impure(u, Continuation.lift((x: u.X) => c(x).flatMap(a1 => a1.fold(a11 => tailRecM(a11)(f), b => pure(b)))), l)

        case ImpureAp(u, c, l) =>
          ImpureAp(u, Continuation.lift(x => c(x).flatMap(a1 => a1.fold(a11 => tailRecM(a11)(f), b => pure(b)))), l)
      }
  }

  final val effApplicativeUnsafe: Applicative[Eff[AnyRef, ?]] = new Applicative[Eff[AnyRef, ?]] {

    def point[A](a: =>A): Eff[AnyRef, A] =
      Pure[AnyRef, A](a)

    def ap[A, B](fa: =>Eff[AnyRef, A])(ff: =>Eff[AnyRef, A => B]): Eff[AnyRef, B] =
      fa match {
        case Pure(a, last) =>
          ff match {
            case Pure(f, last1)                   => Pure(f(a), last1 *> last)
            case Impure(NoEffect(f), c, last1)    => Impure(NoEffect[AnyRef, Any](f), c.append(f1 => pure(f1(a))), c.onNone).addLast(last1 *> last)
            case Impure(u: Union[_, _], c, last1) => ImpureAp(Unions(u, Vector.empty), c.dimapEff((_:Vector[Any]).head)(_.map(_(a))), last1 *> last)
            case ImpureAp(u, c, last1)            => ImpureAp(u, c.map(_(a)), last1 *> last)
          }

        case Impure(NoEffect(a), c, last) =>
          ap(c(a).addLast(last))(ff)

        case Impure(u: Union[_, _], c, last) =>
          ff match {
            case Pure(f, last1)                     => ImpureAp(Unions(u, Vector.empty), c.contramap((_:Vector[Any]).head).map(f), last1 *> last)
            case Impure(NoEffect(f), c1, last1)     => Impure(u, c.append(x => c1(f).map(_(x)))).addLast(last1 *> last)
            case Impure(u1: Union[_, _], c1, last1) => ImpureAp(Unions(u, Vector(u1)),  Continuation.lift(ls => ap(c(ls.head))(c1(ls(1))), c.onNone), last1 *> last)
            case ImpureAp(u1, c1, last1)            => ImpureAp(Unions(u, u1.unions), Continuation.lift(ls => ap(c(ls.head))(c1(ls.drop(1))), c.onNone), last1 *> last)
          }

        case ImpureAp(unions, c, last) =>
          ff match {
            case Pure(f, last1)                    => ImpureAp(unions, c map f, last1 *> last)
            case Impure(NoEffect(f), c1, last1)    => ImpureAp(unions, c.append(x => c1(f).map(_(x)))).addLast(last1 *> last)
            case Impure(u: Union[_, _], c1, last1) => ImpureAp(Unions(unions.first, unions.rest :+ u), Continuation.lift(ls => ap(c(ls.dropRight(1)))(c1(ls.last)), c.onNone), last1 *> last)
            case ImpureAp(u, c1, last1)            => ImpureAp(u append unions, Continuation.lift({ xs =>
              val usize = u.size
              val (taken, dropped) = xs.splitAt(usize)
              // don't recurse if the number of effects is too large
              // this will ensure stack-safety on large traversals
              // and keep enough concurrency on smaller traversals
              if (xs.size > 10)
                Eff.impure(taken, Continuation.lift((xs1: Vector[Any]) => ap(c(dropped))(c1(xs1)), c1.onNone))
              else
                ap(c(dropped))(c1(taken))
            }, c.onNone), last1 *> last)
          }

      }
  }

  implicit final def EffMonad[R]: Monad[Eff[R, ?]] = effMonadUnsafe.asInstanceOf[Monad[Eff[R, ?]]]

  final def EffApplicative[R]: Applicative[Eff[R, ?]] = effApplicativeUnsafe.asInstanceOf[Applicative[Eff[R, ?]]]

}

object EffImplicits extends EffImplicits

trait EffCreation {
  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: T |= R): Eff[R, V] =
    ImpureAp(Unions(member.inject(tv), Vector.empty), Continuation.lift(xs => pure[R, V](xs.head.asInstanceOf[V])))

  /** use the internal effect as one of the stack effects */
  def collapse[R, M[_], A](r: Eff[R, M[A]])(implicit m: M |= R): Eff[R, A] =
    EffMonad[R].flatMap(r)(mx => send(mx)(m))

  /** create an Eff value for () */
  def unit[R]: Eff[R, Unit] =
    EffMonad.pure(())

  /** create a pure value */
  def pure[R, A](a: A): Eff[R, A] =
    Pure(a)

  /** create a impure value from an union of effects and a continuation */
  def impure[R, X, A](union: Union[R, X], continuation: Continuation[R, X, A]): Eff[R, A] =
    Impure[R, X, A](union, continuation)

  /** create a delayed impure value */
  def impure[R, A, B](value: A, continuation: Continuation[R, A, B]): Eff[R, B] =
    Impure(NoEffect(value), continuation)

  /** create a delayed impure value */
  def impure[R, A, B](value: A, continuation: Continuation[R, A, B], map: B => B): Eff[R, B] =
    Impure(NoEffect(value), Continuation.lift((a: A) => continuation(a), continuation.onNone).map(map))

  /** apply a function to an Eff value using the applicative instance */
  def ap[R, A, B](a: Eff[R, A])(f: Eff[R, A => B]): Eff[R, B] =
    EffImplicits.EffApplicative[R].ap(a)(f)

  /** use the applicative instance of Eff to traverse a list of values */
  def traverseA[R, F[_] : Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to sequence a list of values */
  def sequenceA[R, F[_] : Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to traverse a list of values, then flatten it */
  def flatTraverseA[R, F[_], A, B](fs: F[A])(f: A => Eff[R, F[B]])(implicit FT: Traverse[F], FM: Monad[F]): Eff[R, F[B]] =
    FT.flatTraverse[Eff[R, ?], A, B](fs)(f)(EffImplicits.EffApplicative[R], FM)

  /** bracket an action with one last action to execute at the end of the program */
  def bracketLast[R, A, B, C](acquire: Eff[R, A])(use: A => Eff[R, B])(release: A => Eff[R, C]): Eff[R, B] =
    for {
      a <- acquire
      b <- use(a).addLast(release(a).void)
    } yield b

  /** attach a clean-up action to the continuation (if any) */
  def whenStopped[R, A](e: Eff[R, A], action: Last[R]): Eff[R, A] =
    e match {
      case Pure(a, l)        => Pure(a, l)
      case Impure(u, c, l)   => Impure(u,   c.copy(onNone = c.onNone <* action), l)
      case ImpureAp(u, c, l) => ImpureAp(u, c.copy(onNone = c.onNone <* action), l)
    }

  def retryUntil[R, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration], waitFor: FiniteDuration => Eff[R, Unit]): Eff[R, A] =
    e.flatMap { a =>
      if (condition(a))
        Eff.pure(a)
      else
        durations match {
          case Nil =>
            Eff.pure(a)

          case duration :: rest =>
            waitFor(duration) >>
              retryUntil(e, condition, rest, waitFor)
        }
    }

}

object EffCreation extends EffCreation

trait EffInterpretation {

  /**
   * base runner for an Eff value having no effects at all
   * the execution is trampolined using Eval
   */
  def run[A](eff: Eff[NoFx, A]): A = {
    def eval[X](e: Eff[NoFx, X]): Name[X] =
      e match {
        case Pure(a, Last(None)) =>
          Value(a)

        case Pure(a, Last(Some(l))) =>
          eval(l.value).as(a)

        case Impure(NoEffect(a), c, Last(None)) =>
          Need(c(a)).flatMap(eval)

        case Impure(NoEffect(a), c, Last(Some(l))) =>
          Need(c(a)).flatMap(eval).flatMap(res => eval(l.value).as(res))

        case other => sys.error("impossible: cannot run the effects in "+other)
      }

    eval(eff).value
  }

  /**
   * peel-off the only present effect
   */
  def detach[M[_], R, A](eff: Eff[R, A])(implicit monad: Monad[M], m: Member.Aux[M, R, NoFx]): M[A] =
    detachA(Eff.effInto[R, Fx1[M], A](eff))

  /**
   * peel-off the only present effect
   */
  def detach[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M]): M[A] =
    detachA(eff)

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], R, A](eff: Eff[R, A])(implicit monad: Monad[M], applicative: Applicative[M], member: Member.Aux[M, R, NoFx]): M[A] =
    detachA(Eff.effInto[R, Fx1[M], A](eff))(monad, applicative)

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M], applicative: Applicative[M]): M[A] =
    Monad[M].tailrecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => monad.pure(Left(l.value.as(a)))
      case Pure(a, Last(None))    => monad.pure(Right(a))

      case Impure(NoEffect(a), continuation, last) =>
        monad.pure(Left(continuation(a).addLast(last)))

      case Impure(u: Union[_, _], continuation, last) =>
        val ta = u.tagged.valueUnsafe.asInstanceOf[M[A]]
        val result: M[Either[Eff[Fx1[M], A], A]] =
          ta.map(x => Left(Impure(NoEffect(x.asInstanceOf[Any]), continuation, last)))

        last match {
          case Last(Some(l)) =>
            detachA(l.value) >> result

          case Last(None) =>
            result
        }

      case ImpureAp(unions, continuation, last) =>
        val effects = unions.unions.map(_.tagged.valueUnsafe.asInstanceOf[M[Any]])
        val sequenced = Traverse[Vector].sequence(effects)(applicative)

        val result: M[Either[Eff[Fx1[M], A], A]] =
          sequenced.map(xs => Left(Impure(NoEffect(xs), continuation, last)))

        last match {
          case Last(Some(l)) =>
            detachA(l.value) >> result

          case Last(None) =>
            result
        }
    }

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a, Last(Some(l)))     => l.value; Some(a)
      case Pure(a, _)                 => Some(a)
      case Impure(NoEffect(a), c, l)  => runPure(c(a).addLast(l))
      case _                          => None
    }

  /**
   * An Eff[R, A] value can be transformed into an Eff[U, A]
   * value provided that all the effects in R are also in U
   */
  def effInto[R, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U]): Eff[U, A] =
    f(e)

}

object EffInterpretation extends EffInterpretation

