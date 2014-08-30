package org.specs2
package runner

import org.specs2.control.Throwablex._
import org.specs2.control._
import org.specs2.main.Arguments

import scalaz.effect.IO
import scalaz._, Scalaz._

/**
 * reusable actions for Runners
 */
object Runner {

  /**
   * execute some actions and exit with the proper code if 'exist' is true
   */
  def execute(actions: Action[Unit], arguments: Arguments, exit: Boolean) = {
    actions.execute(consoleLogging).unsafePerformIO.fold(
      ok => IO(exitSystem(0, exit)),
      error => error.fold(
        consoleLogging,
        t => logThrowable(t, arguments),
        (m, t) => consoleLogging(m) >> logThrowable(t, arguments) >> IO(exitSystem(100, exit))
      )
    ).unsafePerformIO
  }

  def logThrowable(t: Throwable, arguments: Arguments): IO[Unit] = {
    if (arguments.commandLine.boolOr("silent", false)) {
      consoleLogging("\n"+t.toString+"\n")    >>
      t.chainedExceptions.traverse_(s => consoleLogging("  caused by " + s.toString)) >>
      consoleLogging("\nSTACKTRACE") >>
      t.getStackTrace.toList.traverse_(t => consoleLogging("  "+t.toString)) >>
      t.chainedExceptions.traverse_ { s =>
        consoleLogging("\n  CAUSED BY " + s.toString) >>
          s.getStackTrace.toList.traverse_(t => consoleLogging("  "+t.toString))
      }
    } else IO(())
  }

  def exitSystem(status: Int, exit: Boolean) {
    if (exit) System.exit(status)
  }
}

