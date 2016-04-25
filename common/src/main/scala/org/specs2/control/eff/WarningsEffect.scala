package org.specs2.control
package eff

import Effects._
import scalaz._
import Interpret._

object WarningsEffect {

  trait WarningsTag

  type Warnings[A] = Writer[String, A] @@ WarningsTag

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R](message: String)(implicit m: Member[Warnings, R]): Eff[R, Unit] =
    WriterEffect.tell(message)(Member.untagMember[({type l[X]=Writer[String, X]})#l, R, WarningsTag](m))

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R <: Effects, A](effects: Eff[Warnings |: R, A]): Eff[R, (A, Vector[String])] = {
    val recurse = new StateRecurse[Warnings, A, (A, Vector[String])] {
      type S = Vector[String]
      val init = Vector()

      def apply[X](x: Warnings[X], s: Vector[String]): (X, Vector[String]) =
        Tag.unwrap(x) match {
          case w => (w.run._2, s :+ w.run._1)
        }

      def finalize(a: A, s: Vector[String]): (A, Vector[String]) =
        (a, s)
    }

    interpretState1[R, Warnings, A, (A, Vector[String])]((a: A) => (a, Vector()))(recurse)(effects)
  }

}
