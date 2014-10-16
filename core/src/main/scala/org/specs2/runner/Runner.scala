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
   * Execute some actions and exit with the proper code if 'exist' is true
   */
  def execute(actions: Action[Unit], arguments: Arguments, exit: Boolean) = {
    val (warnings, result) = actions.run(consoleLogging).unsafePerformIO
    result.fold(
      ok => logUserWarnings(warnings)(consoleLogging) >> IO(exitSystem(0, exit)),
      error => error.fold(
        m      => logUserWarnings(warnings)(consoleLogging) >> consoleLogging(m),
        t      => logUserWarnings(warnings)(consoleLogging) >> logThrowable(t, arguments)(consoleLogging),
        (m, t) => logUserWarnings(warnings)(consoleLogging) >> consoleLogging(m) >> logThrowable(t, arguments)(consoleLogging)
      ) >> IO(exitSystem(100, exit))
    ).unsafePerformIO
  }

  /**
   * Use the console logging to log exceptions
   */
  def logThrowable(t: Throwable, arguments: Arguments)(print: String => IO[Unit]): IO[Unit] = {
    if (!arguments.commandLine.boolOr("silent", false)) {
      t match {
        case ActionException(warnings, message, _) =>
          (if (warnings.nonEmpty) print("Warnings:") else IO(())) >>
          warnings.traverseU(print) >>
          message.traverseU(print).void

        case other =>
            print("\n"+t.toString+"\n")    >>
              t.chainedExceptions.traverse_(s => print("  caused by " + s.toString)) >>
              print("\nSTACKTRACE") >>
              t.getStackTrace.toList.traverse_(t => print("  "+t.toString)) >>
              t.chainedExceptions.traverse_ { s =>
                print("\n  CAUSED BY " + s.toString) >>
                  s.getStackTrace.toList.traverse_(t => print("  "+t.toString))
              } >>
              print("\n\nThis looks like a specs2 exception...\nPlease report it with the preceding stacktrace at http://github.com/etorreborre/specs2/issues") >>
              print(" ")

      }
    } else IO(())
  }

  /**
   * Log the issues which might have been caused by the user
   */
  def logUserWarnings(warnings: Vector[String])(print: String => IO[Unit]): IO[Unit] = {
    (if (warnings.nonEmpty) print("Warnings:\n") else IO(())) >>
    warnings.traverseU(print).void
  }

  /**
   * Exit the JVM with a given status
   */
  def exitSystem(status: Int, exit: Boolean) {
    if (exit) System.exit(status)
  }
}

