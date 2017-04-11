package org.specs2.control.eff.syntax

import org.specs2.control.eff._
import org.specs2.control.eff.eval._

object eval extends eval

trait eval {

  implicit class EvalEffectOps[R, A](e: Eff[R, A]) {

    def runEval(implicit member: Member[Eval, R]): Eff[member.Out, A] =
      EvalInterpretation.runEval(e)(member.aux)

    def attemptEval(implicit member: Member[Eval, R]): Eff[member.Out, Throwable Either A] =
      EvalInterpretation.attemptEval(e)(member.aux)

  }

}

