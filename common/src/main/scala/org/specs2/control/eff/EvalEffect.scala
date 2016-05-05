package org.specs2.control.eff

import scala.util.control.NonFatal
import scalaz._
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
  type Eval[A] = Need[A]
}

object EvalTypes extends EvalTypes

trait EvalCreation extends EvalTypes {
  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: => A)(implicit m: Member[Eval, R]): Eff[R, A] =
    send(Need(a))
}

trait EvalInterpretation extends EvalTypes {
  def runEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]) = -\/(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] = {
    val recurse = new Recurse[Eval, U, Throwable \/ A] {
      def apply[X](m: Eval[X]) =
        try { -\/(m.value) }
        catch { case NonFatal(t) => \/-(Eff.pure(-\/(t))) }
    }

    interpret1((a: A) => \/-(a): Throwable \/ A)(recurse)(r)
  }
}

object EvalInterpretation extends EvalInterpretation

