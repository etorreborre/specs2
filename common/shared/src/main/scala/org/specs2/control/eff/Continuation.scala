package org.specs2
package control.eff

import fp._
import scala.annotation.tailrec

/**
 * Sequence of monadic functions from A to B: A => Eff[B]
 *
 * Internally it is represented as a Vector of functions:
 *
 *  A => Eff[R, X1]; X1 => Eff[R, X2]; X2 => Eff[R, X3]; ...; X3 => Eff[R, B]
 *
 * An alternate unit value can also be set on this function in case the argument A is not available.
 * This value can be set by an effect to do some cleanup if it doesn't even get the chance
 * to add its own effect. See SafeEffect.bracket
 *
 */
case class Continuation[R, A, B](functions: Vector[Any => Eff[R, Any]], onNone: Last[R] = Last.none[R]) extends (A => Eff[R, B]) {

  /**
   * append a new monadic function to this list of functions such that
   *
   * Arrs[R, A, B] => (B => Eff[R, C]) => Arrs[R, A, C]
   *
   */
  def append[C](f: B => Eff[R, C]): Continuation[R, A, C] =
    Continuation(functions :+ f.asInstanceOf[Any => Eff[R, Any]], onNone)

  /** map the last returned effect */
  def mapLast[C](f: Eff[R, B] => Eff[R, C]): Continuation[R, A, C] =
    functions match {
      case v if v.isEmpty => Continuation[R, A, C](v :+ ((a: Any) => f(Eff.pure(a.asInstanceOf[B])).asInstanceOf[Eff[R, Any]]), onNone)
      case fs :+ last => Continuation(fs :+ ((x: Any) => f(last(x).asInstanceOf[Eff[R, B]]).asInstanceOf[Eff[R, Any]]), onNone)
    }

  /** map the last value */
  def map[C](f: B => C): Continuation[R, A, C] =
    Continuation(functions :+ ((x: Any) => Eff.pure[R, Any](f(x.asInstanceOf[B]).asInstanceOf[Any])), onNone)

  /**
   * execute this monadic function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any, last: Last[R] = Last.none[R]): Eff[R, B] = {
      fs match {
        case vec if vec.isEmpty =>
          Pure[R, B](v.asInstanceOf[B], last)

        case f +: rest =>
          val fv = f(v)
          if (rest.isEmpty) {
            fv.asInstanceOf[Eff[R, B]].addLast(last)
          } else {
            fv match {
              case Pure(a1, l1) =>
                go(rest, a1, last *> l1)

              case Impure(u, q, l) =>
                Impure[R, u.X, B](u, q.copy(q.functions ++ rest, onNone = q.onNone *> onNone), last *> l)

              case ImpureAp(unions, q, l) =>
                ImpureAp[R, unions.X, B](unions, q.copy(q.functions ++ rest, onNone = q.onNone *> onNone), last *> l)
            }
          }
      }
    }

    go(functions, a)
  }

  def applyEval(a: A): Eff[R, B] =
    Impure(NoEffect(a), Continuation.lift((x: A) => apply(x), onNone))

  def contramap[C](f: C => A): Continuation[R, C, B] =
    Continuation(((c: Any) => Eff.pure[R, Any](f(c.asInstanceOf[C]).asInstanceOf[Any])) +: functions, onNone)

  def contraFlatMap[C](f: C => Eff[R, A]): Continuation[R, C, B] =
    Continuation(f.asInstanceOf[Any => Eff[R, Any]] +: functions, onNone)

  /** adapt the input and output of an Arrs function */
  def dimapEff[C, D](f: C => A)(g: Eff[R, B] => Eff[R, D]): Continuation[R, C, D] =
    Continuation.lift((c: C) => g(apply(f(c))), onNone)

  /** create an Arrs function from the result of another Arrs function */
  def interpret[U, C](map: Eff[R, B] => Eff[U, C])(mapOnNone: Last[R] => Last[U]): Continuation[U, A, C] =
    Continuation.lift((a: A) => map(apply(a)), mapOnNone(onNone))

  /** create an Arrs function from the result of another Arrs function */
  def interpretEff[U, C](map: Eff[R, B] => Eff[U, C])(mapOnNone: Eff[R, Unit] => Eff[U, Unit]): Continuation[U, A, C] =
    Continuation.lift((a: A) => map(apply(a)), onNone.interpret(mapOnNone))

  def transform[U, M[_], N[_]](t: ~>[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, R, U]): Continuation[R, A, B] =
    Continuation(functions.map(f => (x: Any) => Interpret.transform(f(x): Eff[R, Any], t)(m, n)), onNone)

  def runOnNone: Eff[R, Unit] =
    onNone.value.map(_.value).getOrElse(Eff.pure(()))
}

object Continuation {

  /** create an Arrs function from a single monadic function */
  def lift[R, A, B](f: A => Eff[R, B], otherwise: Last[R] = Last.none[R]): Continuation[R, A, B] =
    Continuation(Vector.empty :+ f.asInstanceOf[Any => Eff[R, Any]], otherwise)

  /** create an Arrs function with no effect, which is similar to using an identity a => EffMonad[R].pure(a) */
  def unit[R, A]: Continuation[R, A, A] =
    Continuation(Vector.empty)

  implicit def ArrsFunctor[R, X]: Functor[Continuation[R, X, ?]] = new Functor[Continuation[R, X, ?]] {
    def map[A, B](fa: Continuation[R, X, A])(f: A => B): Continuation[R, X, B] =
      fa.map(f)
  }
}



