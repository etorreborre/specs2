package org.specs2

import control.eff._
import all._
import syntax.all._
import concurrent.ExecutionEnv
import scala.concurrent.duration.FiniteDuration

import scalaz._
import scalaz.effect.IO
import org.specs2.execute.{AsResult, Result}

import scalaz.concurrent.Task
import scalaz.syntax.bind._
import ErrorEffect.{Error, ErrorOrOk, exception, fail}
import ConsoleEffect._
import WarningsEffect._
import org.specs2.control.producer._

package object control {

  /**
   * Actions logging
   */
  type Logger = String => Unit
  lazy val noLogging = (s: String) => ()
  lazy val consoleLogging = (s: String) => println(s)

  type StreamStack = Fx.fx2[Async, Safe]
  type ActionStack   = Fx.fx5[ErrorOrOk, Console, Warnings, Safe, Async]

  type Action[A] = Eff[ActionStack, A]
  type AsyncStream[A] = Producer[ActionStack, A]
  type AsyncTransducer[A, B] = Transducer[ActionStack, A, B]

  type AsyncSink[A] = origami.Fold[ActionStack, A, Unit]

  def emitAsync[A](as: A*): AsyncStream[A] =
    producer.producers.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    producer.producers.eval(AsyncCreation.asyncDelay(a))

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: ErrorOrOk <= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def executeAction[A](action: Action[A], printer: String => Unit = s => ()): (Error \/ A, List[String]) = {
    type S = Fx.append[Fx.fx2[ErrorOrOk, Console], Fx.fx2[Warnings, Async]]

    action.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
       runError.runConsoleToPrinter(printer).runWarnings.runAsyncTask.run
  }

  def runAction[A](action: Action[A], printer: String => Unit = s => ()): Error \/ A =
    attemptExecuteAction(action, printer).fold(
      t => -\/(-\/(t)),
      other => other._1)

  def withTimeout[A](action: Action[A])(timeout: FiniteDuration, env: ExecutionEnv): Action[A] =
    interpret.interceptNat[ActionStack, Async, A](action)(new (Async ~> Async) {
      def apply[X](tx: Async[X]) =
        tx match {
          case AsyncTask(t) => AsyncTask(t.unsafePerformTimed(timeout)(env.scheduledExecutorService))
        }
    })


  def attemptAction[A](action: Action[A], printer: String => Unit = s => ()): Throwable \/ A =
    runAction(action, printer) match {
      case -\/(-\/(t)) => -\/(t)
      case -\/(\/-(f)) => -\/(new Exception(f))
      case \/-(a)      => \/-(a)
    }

  def attemptExecuteAction[A](action: Action[A], printer: String => Unit = s => ()): Throwable \/ (Error \/ A, List[String]) =
    action.runError.runConsoleToPrinter(printer).runWarnings.execSafe.runAsyncTask.run

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

    def log(m: String, doIt: Boolean = true): Action[Unit] =
      ConsoleEffect.log(m, doIt)

    def logThrowable[R :_console](t: Throwable, doIt: Boolean = true): Eff[R, Unit] =
      ConsoleEffect.logThrowable(t, doIt)

    def logThrowable[R :_console](t: Throwable): Eff[R, Unit] =
      ConsoleEffect.logThrowable(t)

    def warn(m: String): Action[Unit] =
      WarningsEffect.warn(m)

    def unit: Action[Unit] =
      ok(())

    def ok[A](a: A): Action[A] =
      ErrorEffect.ok(a)

    def protect[A](a: =>A): Action[A] =
      SafeEffect.protect(a)

    def asyncDelay[A](a: =>A): Action[A] =
      AsyncCreation.asyncDelay(a)

    def delayed[A](a: =>A): Action[A] =
      ErrorEffect.ok(a)

    def fail[A](message: String): Action[A] =
      ErrorEffect.fail[ActionStack, A](message)

    def exception[A](t: Throwable): Action[A] =
      ErrorEffect.exception[ActionStack, A](t)

    def checkThat[A](a: =>A, condition: Boolean, failureMessage: String): Action[A] =
      delayed(a).flatMap { value =>
        if (condition) delayed(value)
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
