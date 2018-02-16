package org.specs2.control
package eff
package syntax

object writer extends writer

trait writer {

  implicit class WriterEffectOps[R, A](e: Eff[R, A]) {

    def runWriter[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, List[O])] =
      WriterInterpretation.runWriter(e)(member.aux)

    def runWriterNoLog[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, A] =
      runWriter[O](member).map(_._1)

    def runWriterLog[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, List[O]] =
      runWriter[O](member).map(_._2)

    def runWriterFold[O, B](fold: RightFold[O, B])(implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, B)] =
      WriterInterpretation.runWriterFold(e)(fold)(member.aux)

    def runWriterUnsafe[O](f: O => Unit)(implicit member: Member[Writer[O, ?], R]): Eff[member.Out, A] =
      WriterInterpretation.runWriterUnsafe(e)(f)(member.aux)

  }

}


