package org.specs2.control.eff

import Eff._
import Effects._
import scalaz._, Scalaz._
import Interpret._

object ConsoleEffect {

  trait ConsoleTag

  type Console[A] = Writer[String, A] @@ ConsoleTag

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) WriterEffect.tell(message)(Member.untagMember[({type l[X]=Writer[String, X]})#l, R, ConsoleTag](m))
    else      EffMonad.point(())

  def logThrowable[R](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else      EffMonad.point(())

  def logThrowable[R](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(m) >>
    log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    EffMonad.point(()))


  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R <: Effects, A](w: Eff[Console |: R, A]): Eff[R, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R <: Effects, A](printer: String => Unit): Eff[Console |: R, A] => Eff[R, A] = {
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

    interpretState1((a: A) => a)(recurse)
  }

}
