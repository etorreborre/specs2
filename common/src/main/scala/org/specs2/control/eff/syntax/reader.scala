package org.specs2.control.eff.syntax

import scalaz._
import org.specs2.control.eff._

object reader extends reader

trait reader {

  implicit class ReaderEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runReader[C, U <: Effects](c: C)(implicit member: Member.Aux[({type l[X]=Reader[C, X]})#l, R, U]): Eff[U, A] =
      ReaderInterpretation.runReader(c)(e)

    def runReaderTagged[C, U <: Effects, T](c: C)(implicit member: Member.Aux[({type l[X] = Reader[C, X] @@ T})#l, R, U]): Eff[U, A] =
      ReaderInterpretation.runReaderTagged(c)(e)

    def localReader[BR, U, C, B](getter: B => C)(implicit m1: Member.Aux[({type l[X]=Reader[C, X]})#l, R, U], m2: Member.Aux[({type l[X]=Reader[B, X]})#l, BR, U]): Eff[BR, A] =
      ReaderInterpretation.localReader[R, BR, U, C, B, A](e, getter)
  }

}

