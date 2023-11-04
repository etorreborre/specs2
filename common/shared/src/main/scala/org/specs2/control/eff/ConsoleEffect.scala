package org.specs2.control.eff

import org.specs2.fp.syntax._
import eff._, writer._
import syntax.writer._

object ConsoleEffect {

  case class ConsoleMessage(value: String) extends AnyVal {
    // this is necessary to avoid a weird ClassCastException
    // class org.specs2.control.eff.ConsoleEffect$ConsoleMessage cannot be cast to class java.lang.String (org.specs2.control.eff.ConsoleEffect$ConsoleMessage is in unnamed module of loader 'app'; java.lang.String is in module java.base of loader 'bootstrap') (JProcedure1.java:15)
    override def toString(): String = value
  }

  type _console[R] = Console |= R
  type Console[A] = Writer[ConsoleMessage, A]


  def log[R :_console](message: String, doIt: Boolean = true): Eff[R, Unit] =
    if (doIt) tell(ConsoleMessage(message))
    else      pure(())

  def logThrowable[R :_console](t: Throwable, doIt: Boolean = true): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else      pure(())

  def logThrowable[R :_console](t: Throwable): Eff[R, Unit] =
    log(t.getMessage) >>
      log(t.getStackTrace.mkString("\n")) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    pure(()))

  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R, U, A](w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
    runConsoleToPrinter(println)(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R, U, A](printer: String => Unit)(w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
    w.runWriterUnsafe((message: ConsoleMessage) => printer(message.toString()))
}

case class Writer[O, A](value: O, a: A) {
  val run: (O, A) =
    (value, a)
}
