package org.specs2.control.eff
package syntax

import ConsoleEffect._

object console extends console

trait console {

  implicit class ConsoleEffectOps[R, A](action: Eff[R, A]) {

    def runConsole[U](implicit m: Member.Aux[Console, R, U]): Eff[U, A] =
      ConsoleEffect.runConsole(action)

    def runConsoleToPrinter[U](printer: String => Unit)(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
      ConsoleEffect.runConsoleToPrinter(printer)(action)
  }

}

