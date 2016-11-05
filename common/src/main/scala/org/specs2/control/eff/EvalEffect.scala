package org.specs2.control.eff

import scala.util.control.NonFatal
import scalaz._, Scalaz._
import Eff._
import Interpret._

/**
 * Effect for delayed computations
 *
 * uses scalaz.Need as a supporting data structure
 */
trait EvalEffect extends
  EvalTypes with
  EvalCreation with
  EvalInterpretation

object EvalEffect extends EvalEffect

trait EvalTypes {
  type Eval[A] = scalaz.Name[A]
  type _Eval[R] = Eval <= R
  type _eval[R] = Eval |= R
}

object EvalTypes extends EvalTypes

trait EvalCreation extends EvalTypes {
  def now[R :_eval, A](a: A): Eff[R, A] =
    pure(a)

  def delay[R :_eval, A](a: => A): Eff[R, A] =
    send(Name(a))
}

trait EvalInterpretation extends EvalTypes {
  def runEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]) = Left(m.value)
      def applicative[X, T[_] : Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] = Right(ms.sequence)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Eval, U, Throwable \/ A] {
      def apply[X](m: Eval[X]) =
        try Left(m.value)
        catch { case NonFatal(t) => Right(Eff.pure(-\/(t))) }

      def applicative[X, T[_] : Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] = Right(ms.sequence)
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }
}

object EvalInterpretation extends EvalInterpretation

