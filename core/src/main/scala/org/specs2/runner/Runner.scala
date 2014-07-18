package org.specs2
package runner

import org.specs2.control.Throwablex._
import org.specs2.control._

import scalaz.effect.IO
import scalaz._, Scalaz._

/**
 * reusable actions for Runners
 */
object Runner {
  def execute(actions: Action[Unit]) = {
    actions.execute(consoleLogging).unsafePerformIO.fold(
      ok => IO(exitSystem(0)),
      error => error.fold(
        consoleLogging,
        logThrowable,
        (m, t) => consoleLogging(m) >> logThrowable(t) >> IO(exitSystem(100))
      )
    ).unsafePerformIO
  }

  def logThrowable(t: Throwable): IO[Unit] = {
    consoleLogging("\n"+t.toString+"\n")    >>
    t.chainedExceptions.traverse_(s => consoleLogging("  caused by " + s.toString)) >>
    consoleLogging("\nSTACKTRACE") >>
    t.getStackTrace.toList.traverse_(t => consoleLogging("  "+t.toString)) >>
    t.chainedExceptions.traverse_ { s =>
      consoleLogging("\n  CAUSED BY " + s.toString) >>
      s.getStackTrace.toList.traverse_(t => consoleLogging("  "+t.toString))
    }
  }

  def exitSystem(status: Int) {
    System.exit(status)
  }
}

