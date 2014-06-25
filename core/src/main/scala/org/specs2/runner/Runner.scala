package org.specs2
package runner

import control._
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import scalaz.syntax.traverse.ToTraverseOps
import scalaz.std.list._
import Throwablex._

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
    consoleLogging(t.getMessage+"\n") >>
      (t :: t.chainedExceptions).map { s =>
        consoleLogging("  caused by " + s.toString) >>
          s.getStackTrace.toList.traverseU(t => consoleLogging("  " + t.toString))
      }.sequence.void
  }

  def exitSystem(status: Int) {
    System.exit(status)
  }
}

