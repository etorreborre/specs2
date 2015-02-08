package org.specs2

import org.specs2.control.ActionT

import scalaz.{WriterT, Monoid}
import scalaz.{Monad, Monoid}
import scalaz.std.anyVal._
import scalaz.effect._
import org.specs2.execute.{AsResult, Result}
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.syntax.bind._

package object control {
  import StatusT._

  /**
   * Actions logging
   */
  type Logger = String => IO[Unit]
  lazy val noLogging = (s: String) => IO(())
  lazy val consoleLogging = (s: String) => IO(println(s))

  /**
   * Action type, using a logger as a reader and no writer
   */
  type Action[A] = ActionT[IO, Logs, Logger, A]

  object Actions extends ActionTSupport[IO, Logs, Logger] {
    def unit: Action[Unit] = empty(implicitly[Monad[IO]], implicitly[Monoid[Logs]])
  }

  type Logs = Vector[String]

  implicit def LogsMonoid: Monoid[Logs] = new Monoid[Logs] {
    def zero: Logs = Vector[String]()
    def append(f1: Logs, f2: =>Logs): Logs = f1 ++ f2
  }

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn(message: String): Action[Unit] =
    ActionT.append(Vector(message))

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[A](message: String, failureMessage: String): Action[A] =
    warn(message) >> ActionT.fail[IO, Logs, Logger, A](failureMessage)

  /** log a value, using the logger coming from the Reader environment */
  def log[R](r: R): Action[Unit] =
    Actions.ask.flatMap(logger => Actions.fromIO(logger(r.toString)))

  /** log a Throwable with its stacktrace and cause, using the logger coming from the Reader environment */
  def logThrowable(t: Throwable, verbose: Boolean): Action[Unit] =
    if (verbose) logThrowable(t) else Actions.unit

  def logThrowable(t: Throwable): Action[Unit] =
    log(t.getMessage) >>
    log(t.getStackTrace.mkString("\n")) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    Actions.unit)

  /** log a value, using the logger coming from the Reader environment, only if verbose is true */
  def log[R](r: R, verbose: Boolean): Action[Unit] =
    if (verbose) log(r) else Actions.unit

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
   * This implicit allows an IOAction[result] to be used inside an example.
   *
   * For example to read a database.
   */
  implicit def ioActionResultAsResult[T : AsResult]: AsResult[Action[T]] = new AsResult[Action[T]] {
    def asResult(ioAction: =>Action[T]): Result =
      ioAction.execute(noLogging).unsafePerformIO.foldAll(
        ok        => AsResult(ok),
        fail      => org.specs2.execute.Failure(fail),
        throwable => org.specs2.execute.Error(throwable),
        (m, t)    => org.specs2.execute.Error(m, t)
      )
  }

  /**
   * An Action[T] can be converted to a Task[T]
   */
  implicit class ioActionToTask[T](action: Action[T]) {
    def toTask = Task.delay {
      action.run(noLogging).unsafePerformIO
     }.flatMap { case (warnings, result) =>
        result.fold(
          t => Task.now(t),
          error => error.fold(
            s      => Task.fail(ActionException(warnings, Some(s), None)),
            t      => Task.fail(ActionException(warnings, None, Some(t))),
            (s, t) => Task.fail(ActionException(warnings, Some(s), Some(t)))
          ))
    }
  }

  /**
   * An Action[T] can be converted to a Task[T] then to a Process[T] returning just one element
   */
  implicit class ioActionToProcess[T](action: Action[T]) {
    def toProcess = Process(action.toTask).eval
  }

  /**
   * execute an action with no logging and return an option
   */
  implicit class ioActionToOption[T](action: Action[T]) {
    def runOption = action.toTask.attemptRun.toOption
  }

  /**
   * A Task[T] (the result of running a Process[Task, T] for example) can be converted to
   * an Action[T]
   */
  implicit class taskToAction[T](task: Task[T]) {
    def toAction = Actions.fromTask(task)
  }

}

