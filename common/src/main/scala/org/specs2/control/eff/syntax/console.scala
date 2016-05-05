package org.specs2.control.eff.syntax

import org.specs2.control.eff._

object console extends console

trait console {

  implicit class ConsoleEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runConsole[U <: Effects](implicit member: Member.Aux[Console, R, U]): Eff[U, A] =
      ConsoleEffect.runConsole(e)

    def runConsoleToPrinter[U <: Effects](printer: String => Unit)(implicit m : Member.Aux[Console, R, U]) =
      ConsoleEffect.runConsoleToPrinter(printer)(e)

  }

}
