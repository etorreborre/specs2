package org.specs2
package runner

import fp.*, syntax.*
import control.*, Throwablex.*
import control.*
import reporter.*
import specification.core.*
import specification.process.Stats
import main.Arguments

/**
 * reusable actions for Runners
 */
object Runner:

  /**
   * Execute some actions and exit with the proper code if 'exit' is true
   */
  def execute(action: Action[Stats], env: Env, exit: Boolean): Unit =
    val logger = env.systemLogger

    action.attempt.runAction(env.specs2ExecutionEnv) match
      case Left(t) =>
        logger.exception(t).runVoid

      case Right(Left(t)) =>
        logger.exception(t).runVoid
        exitSystem(1, exit)

      case Right(Right(result)) =>
        if result.isSuccess then
           exitSystem(0, exit)
        else
          exitSystem(1, exit)



  /**
   * Exit the JVM with a given status
   */
  def exitSystem(status: Int, exit: Boolean): Unit =
    if exit then System.exit(status)


case class RunnerLogger(env: Env):

  val arguments: Arguments =
    env.arguments

  val logger: PrinterLogger =
    env.printerLogger

  /**
   * Use the console logging to log exceptions
   */
  def logThrowable(t: Throwable): Operation[Unit] =

    when (!arguments.commandLine.boolOr("silent", false)) {
      t match
      case UserException(m, throwable) => logException(m, throwable)

      case ActionException(warnings, message, exception) =>
        if warnings.nonEmpty then print("Warnings:\n") >> print(warnings.mkString("", "\n", "\n"))
        else Operation.unit >>
          message.traverse(print).void >>
          exception.traverse(e => logException(e.getMessage, e)).void

      case _: InterruptedException => print("User cancellation. Bye")

      case other =>
        print("\n" + t.toString + "\n") >>
          logStack(t) >>
        print("\n\nThis looks like a specs2 exception...\nPlease report it with the preceding stacktrace at http://github.com/etorreborre/specs2/issues") >>
          print(" ")

    }

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

  /**
   * Log the issues which might have been caused by the user
   */
  def logUserWarnings(warnings: List[String]): Operation[Unit] =
    when(warnings.nonEmpty)(print("Warnings:\n")) >>
      warnings.traverse(print).void

  private def print(m: String): Operation[Unit] =
    Operation.delayed(logger.errorLog(m))
