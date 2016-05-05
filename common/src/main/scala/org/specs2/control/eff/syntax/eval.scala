package org.specs2.control.eff.syntax

import scalaz._
import org.specs2.control.eff._
import EvalEffect._

object eval extends eval

trait eval {

  implicit class EvalEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runEval[U <: Effects](implicit member: Member.Aux[Eval, R, U]): Eff[U, A] =
      EvalInterpretation.runEval(e)

    def attemptEval[U <: Effects](implicit member: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] =
      EvalInterpretation.attemptEval(e)

  }

}

