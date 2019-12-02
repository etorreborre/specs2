package org.specs2
package runner

import fp._, syntax._
import control._, Throwablex._
import control._
import specification.core._
import specification.process.Stats
import main.Arguments

/**
 * reusable actions for Runners
 */
object Runner {

  /**
   * Execute some actions and exit with the proper code if 'exit' is true
   */
  def execute(action: Action[Stats], arguments: Arguments, exit: Boolean)(env: Env): Unit = {
    val logger = ConsoleLogger()

    action.attempt.runAction(env.specs2ExecutionEnv) match {
      case Left(t) =>
        logger.exception(t).runVoid

      case Right(Left(t)) =>
        logger.exception(t).runVoid
        exitSystem(1, exit)

      case Right(Right(result)) =>
        if (result.isSuccess)
           exitSystem(0, exit)
        else
          exitSystem(1, exit)
    }
  }

  /**
   * Use the console logging to log exceptions
   */
  def logThrowable(t: Throwable, arguments: Arguments)(print: String => Name[Unit]): Name[Unit] = {
    def logStack(exception: Throwable) =
      exception.chainedExceptions.traverse_(s => print("  caused by " + s.toString)) >>
        print("\nSTACKTRACE") >>
        exception.getStackTrace.toList.traverse_(e => print("  " + e.toString)) >>
        exception.chainedExceptions.traverse_ { s =>
          print("\n  CAUSED BY " + s.toString) >> s.getStackTrace.toList.traverse_(e => print("  " + e.toString))
        }

     def logException(m: String, throwable: Throwable) =
       print("\n" + m + "\n") >>
         logStack(throwable) >>
         print(" ")

    if (!arguments.commandLine.boolOr("silent", false)) {
      t match {
      case UserException(m, throwable) => logException(m, throwable)

      case ActionException(warnings, message, exception) =>
        if (warnings.nonEmpty) print("Warnings:\n") >> print(warnings.mkString("", "\n", "\n"))
        else Name(()) >>
          message.traverse(print).void >>
          exception.traverse(e => logException(e.getMessage, e)).void

      case _: InterruptedException => print("User cancellation. Bye")

      case other =>
        print("\n" + t.toString + "\n") >>
          logStack(t) >>
        print("\n\nThis looks like a specs2 exception...\nPlease report it with the preceding stacktrace at http://github.com/etorreborre/specs2/issues") >>
          print(" ")

      }
    } else Name(())
  }

  /**
   * Log the issues which might have been caused by the user
   */
  def logUserWarnings(warnings: List[String])(print: String => Name[Unit]): Name[Unit] = {
    (if (warnings.nonEmpty) print("Warnings:\n") else Name(())) >>
      warnings.traverse(print).void
  }

  /**
   * Exit the JVM with a given status
   */
  def exitSystem(status: Int, exit: Boolean): Unit = {
    if (exit) System.exit(status)
  }

}
