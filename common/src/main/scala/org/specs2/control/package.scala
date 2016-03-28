package org.specs2

import scalaz._
import scalaz.effect.IO
import org.specs2.execute.{AsResult, Result}
import scalaz.concurrent.Task
import scalaz.syntax.bind._

import control.eff._
import ErrorEffect._
import WarningsEffect._
import ConsoleEffect._
import Effects._
import Eff.run
import EvalEffect._
import Member.{<=}

package object control {

  /**
   * Actions logging
   */
  type Logger = String => Unit
  lazy val noLogging = (s: String) => ()
  lazy val consoleLogging = (s: String) => println(s)

  /**
   * Action type, using a logger as a reader and no writer
   */
  type ActionStack = ErrorOrOk |: Console |: Warnings |: Eval |: NoEffect

  implicit def EvalMember: Member[Name, ActionStack] =
    Member.MemberNatIsMember

  implicit def WarningsMember: Member[Warnings, ActionStack] =
    Member.MemberNatIsMember

  implicit def ConsoleMember: Member[Console, ActionStack] =
    Member.MemberNatIsMember

  implicit def ErrorMember: Member[ErrorOrOk, ActionStack] =
    Member.MemberNatIsMember

  type Action[A] = Eff[ActionStack, A]

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: ErrorOrOk<= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def executeAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Error \/ A, Vector[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runError(action)))))

  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): Error \/ A =
    attemptExecuteAction(action, printer).fold(
      t => -\/(-\/(t)),
      other => other._1)

  def attemptExecuteAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): Throwable \/ (Error \/ A, Vector[String]) =
    run(attemptEval(runWarnings(runConsoleToPrinter(printer)(runError(action)))))

  /**
   * This implicit allows any IO[Result] to be used inside an example:
   *
   * "this should work" in {
   *   IO(success)
   * }
   */
  implicit def ioResultAsResult[T : AsResult]: AsResult[IO[T]] = new AsResult[IO[T]] {
    def asResult(io: =>IO[T]) = AsResult(io.unsafePerformIO())
  }

  /**
   * This implicit allows an Action[result] to be used inside an example.
   *
   * For example to read a database.
   */
  implicit def actionAsResult[T : AsResult]: AsResult[Action[T]] = new AsResult[Action[T]] {
    def asResult(action: =>Action[T]): Result =
      runAction(action).fold(
        err => err.fold(t => org.specs2.execute.Error(t), f => org.specs2.execute.Failure(f)),
        ok => AsResult(ok)
      )
  }

  /**
   * An Action[T] can be converted to a Task[T]
   */
  implicit class actionToTask[T](action: Action[T]) {
    def toConsoleTask =
      action.toTask(println)

    def toTask(logger: String => Unit) =
      Task.delay(executeAction(action)).
      flatMap { case (result, warnings) =>
        result.fold(
          error => error.fold(
            t      => Task.fail(ActionException(warnings, None, Some(t))),
            s      => Task.fail(ActionException(warnings, Some(s), None))),

          t => Task.delay(warnings.foreach(w => logger(w+"\n"))) >> Task.now(t))
    }
  }

  implicit class actionOps[T](action: Action[T]) {
    def when(condition: Boolean): Action[Unit] =
      if (condition) action.as(()) else Actions.ok(())

    def unless(condition: Boolean): Action[Unit] =
      action.when(!condition)

    def whenFailed(error: Error => Action[T]): Action[T] =
      Actions.whenFailed(action, error)

    def |||(other: Action[T]): Action[T] =
      Actions.orElse(action, other)

    def orElse(other: Action[T]): Action[T] =
      Actions.orElse(action, other)
  }

  /**
   * execute an action with no logging and return an option
   */
  implicit class ioActionToOption[T](action: Action[T]) {
    def runOption = runAction(action).toOption
  }

  /**
   * A Task[T] (the result of running a Process[Task, T] for example) can be converted to
   * an Action[T]
   */
  implicit class taskToAction[T](task: Task[T]) {
    def toAction: Action[T] =
      task.attemptRun.fold(t => exception(t), a => ErrorEffect.ok(a))

    def runOption: Option[T] =
      task.get.run.toOption
  }


  object Actions {

    def unit: Action[Unit] =
      ok(())

    def ok[A](a: A): Action[A] =
      ErrorEffect.ok(a)

    def safe[A](a: =>A): Action[A] =
      ErrorEffect.ok(a)

    def fail[A](message: String): Action[A] =
      ErrorEffect.fail[ActionStack, A](message)

    def exception[A](t: Throwable): Action[A] =
      ErrorEffect.exception[ActionStack, A](t)

    def checkThat[A](a: =>A, condition: Boolean, failureMessage: String): Action[A] =
      safe(a).flatMap { value =>
        if (condition) safe(value)
        else           fail(failureMessage)
      }

    def fromTask[A](task: Task[A]): Action[A] =
      task.toAction

    def fromError[A](error: ErrorEffect.Error): Action[A] =
      ErrorEffect.error(error)

    def orElse[A](action1: Action[A], action2: Action[A]): Action[A] =
      ErrorEffect.orElse(action1, action2)

    def whenFailed[A](action: Action[A], onError: Error => Action[A]): Action[A] =
      ErrorEffect.whenFailed(action, onError)

  }
}
