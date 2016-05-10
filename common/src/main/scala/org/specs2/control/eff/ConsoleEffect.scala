package org.specs2.control.eff

import Eff._
import Interpret._
import scalaz.syntax.all._
import scalaz._
import Effects._

trait ConsoleEffect {

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) WriterEffect.tell(message)(Member.untagMember[({type l[X]=Writer[String, X]})#l, R, ConsoleTag](m))
    else      EffMonad.point(())

  def logThrowable[R](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else      EffMonad.pure(())

  def logThrowable[R](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(m) >>
    log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    EffMonad.pure(()))


  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R <: Effects, U <: Effects, A](w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R <: Effects, U <: Effects, A](printer: String => Unit)(w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]) = {
    val recurse = new StateRecurse[Console, A, A] {
      type S = Unit
      val init = ()

      def apply[X](x: Console[X], s: Unit): (X, Unit) =
        Tag.unwrap(x) match {
          case w => (w.run._2, printer(w.run._1))
        }

      def finalize(a: A, s: Unit): A =
        a
    }

    interpretState1((a: A) => a)(recurse)(w)
  }

}

object ConsoleEffect extends ConsoleEffect

trait ConsoleImplicits extends ConsoleImplicits1 {
  implicit def TaggedConsoleMemberZero[Tg]: Member.Aux[({type l[X] = Console[X] @@ Tg})#l, ({type l[X] = Console[X] @@ Tg})#l |: NoEffect, NoEffect] = {
    type T[X] = Console[X] @@ Tg
    Member.zero[T]
  }

  implicit def TaggedConsoleMemberFirst[R <: Effects, Tg]: Member.Aux[({type l[X] = Console[X] @@ Tg})#l, ({type l[X] = Console[X] @@ Tg})#l |: R, R] = {
    type T[X] = Console[X] @@ Tg
    Member.first[T, R]
  }
}

trait ConsoleTag

trait ConsoleImplicits1 {
  implicit def TaggedConsoleMemberSuccessor[O[_], R <: Effects, U <: Effects, Tg](implicit m: Member.Aux[({type l[X] = Console[X] @@ Tg})#l, R, U]): Member.Aux[({type l[X] = Console[X] @@ Tg})#l, O |: R, O |: U] = {
    type T[X] = Console[X] @@ Tg
    Member.successor[T, O, R, U]
  }
}

object ConsoleImplicits extends ConsoleImplicits
