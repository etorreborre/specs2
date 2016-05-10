package org.specs2.control.eff

import org.specs2.control.eff.Effects._

import scalaz._

trait WarningsEffect {

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R](message: String)(implicit m: Member[Warnings, R]): Eff[R, Unit] =
    WriterEffect.tell(message)(Member.untagMember[({type l[X]=Writer[String, X]})#l, R, WarningsTag](m))

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R <: Effects, U <: Effects, A](w: Eff[R, A])(implicit m: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
    WriterEffect.runWriterTagged[R, U, WarningsTag, String, A](w)

}

object WarningsEffect  extends WarningsEffect


trait WarningsImplicits extends WarningsImplicits1 {
  implicit def TaggedWarningsMemberZero[Tg]: Member.Aux[({type l[X] = Warnings[X] @@ Tg})#l, ({type l[X] = Warnings[X] @@ Tg})#l |: NoEffect, NoEffect] = {
    type T[X] = Warnings[X] @@ Tg
    Member.zero[T]
  }

  implicit def TaggedWarningsMemberFirst[R <: Effects, Tg]: Member.Aux[({type l[X] = Warnings[X] @@ Tg})#l, ({type l[X] = Warnings[X] @@ Tg})#l |: R, R] = {
    type T[X] = Warnings[X] @@ Tg
    Member.first[T, R]
  }
}

trait WarningsImplicits1 {
  implicit def TaggedWarningsMemberSuccessor[O[_], R <: Effects, U <: Effects, Tg](implicit m: Member.Aux[({type l[X] = Warnings[X] @@ Tg})#l, R, U]): Member.Aux[({type l[X] = Warnings[X] @@ Tg})#l, O |: R, O |: U] = {
    type T[X] = Warnings[X] @@ Tg
    Member.successor[T, O, R, U]
  }
}

object WarningsImplicits extends WarningsImplicits

trait WarningsTag
