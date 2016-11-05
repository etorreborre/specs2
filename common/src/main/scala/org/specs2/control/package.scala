package org.specs2

import java.util.concurrent.TimeUnit

import control.eff._
import all._
import syntax.all._
import concurrent.ExecutionEnv

import scala.concurrent.duration.{Duration, FiniteDuration}
import scalaz._
import scalaz.effect.IO
import org.specs2.execute.{AsResult, Result}

import scalaz.syntax.bind._
import ErrorEffect.{Error, ErrorOrOk, exception, fail}
import ConsoleEffect._
import WarningsEffect._
import org.specs2.control.producer._

import scala.concurrent._
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global

package object control {

  /**
   * Actions logging
   */
  type Logger = String => Unit
  lazy val noLogging = (s: String) => ()
  lazy val consoleLogging = (s: String) => println(s)

  type StreamStack = Fx.fx2[Async, Safe]
  type ActionStack = Fx.fx5[ErrorOrOk, Console, Warnings, Safe, Async]
  type OperationStack = Fx.fx4[ErrorOrOk, Console, Warnings, Safe]

  type Action[A] = Eff[ActionStack, A]
  type Operation[A] = Eff[OperationStack, A]
  
  type AsyncStream[A] = Producer[ActionStack, A]
  type AsyncTransducer[A, B] = Transducer[ActionStack, A, B]

  type AsyncFold[A, B] = origami.Fold[ActionStack, A, B]
  type AsyncSink[A] = origami.Fold[ActionStack, A, Unit]


  val asyncService = AsyncFutureService.create

  def emitAsync[A](as: A*): AsyncStream[A] =
    producer.producers.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    producer.producers.eval(asyncService.asyncDelay(a))

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: ErrorOrOk <= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def executeAction[A](action: Action[A], printer: String => Unit = s => ()): (Error \/ A, List[String]) = {
    type S = Fx.append[Fx.fx2[ErrorOrOk, Console], Fx.fx2[Warnings, Async]]

    Await.result(action.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
      runError.runConsoleToPrinter(printer).runWarnings.runAsyncFuture, Duration.Inf)
  }

  def runAction[A](action: Action[A], printer: String => Unit = s => ()): Error \/ A =
    attemptExecuteAction(action, printer).fold(
      t => -\/(-\/(t)),
      other => other._1)

  def runOperation[A](operation: Operation[A], printer: String => Unit = s => ()): Error \/ A =
    attemptExecuteOperation(operation, printer).fold(
      t => -\/(-\/(t)),
      other => other._1)

  def executeOperation[A](operation: Operation[A], printer: String => Unit = s => ()): (Error \/ A, List[String]) = {
    type S = Fx.fx3[ErrorOrOk, Console, Warnings]

    operation.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
      runError.runConsoleToPrinter(printer).runWarnings.run
  }

  def withTimeout[A](action: Action[A])(timeout: FiniteDuration, env: ExecutionEnv): Action[A] =
    interpret.interceptNat[ActionStack, Async, A](action)(new (Async ~> Async) {
      def apply[X](tx: Async[X]) =
        tx match {
          case AsyncFutureNow(a)     => AsyncFutureNow(a)
          case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
          case AsyncFutureDelayed(a) => apply(AsyncFuture(env.executionContext, { ec => Future(a())(ec) }))

          case AsyncFuture(ecx, x) => AsyncFuture(ecx, { implicit ec =>
            lazy val f = x(ec)
            if (timeout.isFinite && timeout.length < 1) {
              try f catch { case NonFatal(t) => Future.failed(t) }
            } else {
              val p = Promise[X]()
              val r = new Runnable {
                def run: Unit = {
                  p completeWith { try f catch { case NonFatal(t) => Future.failed(t) } }
                  ()
                }
              }
              env.scheduledExecutorService.schedule(r, timeout.toMillis, TimeUnit.MILLISECONDS)
              p.future
            }
          })
        }
    })

  def attemptAction[A](action: Action[A], printer: String => Unit = s => ()): Throwable \/ A =
    runAction(action, printer) match {
      case -\/(-\/(t)) => -\/(t)
      case -\/(\/-(f)) => -\/(new Exception(f))
      case \/-(a)      => \/-(a)
    }

  def attemptExecuteAction[A](action: Action[A], printer: String => Unit = s => ()): Throwable \/ (Error \/ A, List[String]) =
    Await.result(action.runError.runConsoleToPrinter(printer).runWarnings.execSafe.runAsyncFuture, Duration.Inf)

  def attemptExecuteOperation[A](operation: Operation[A], printer: String => Unit = s => ()): Throwable \/ (Error \/ A, List[String]) =
    operation.runError.runConsoleToPrinter(printer).runWarnings.execSafe.run

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
   * This implicit allows an Operation[result] to be used inside an example.
   */
  implicit def operationAsResult[T : AsResult]: AsResult[Operation[T]] = new AsResult[Operation[T]] {
    def asResult(operation: =>Operation[T]): Result =
      runOperation(operation).fold(
        err => err.fold(t => org.specs2.execute.Error(t), f => org.specs2.execute.Failure(f)),
        ok => AsResult(ok)
      )
  }

  implicit class actionOps[T](action: Action[T]) {
    def run(implicit e: Monoid[T]): T =
      runAction(action, println) match {
        case \/-(a) => a
        case -\/(t) => println("error while interpreting an action "+t.fold(Throwables.render, f => f)); Monoid[T].zero
      }

    def runOption: Option[T] =
      runAction(action, println) match {
        case \/-(a) => Option(a)
        case -\/(t) => println("error while interpreting an action "+t.fold(Throwables.render, f => f)); None
      }

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

  implicit class operationOps[T](operation: Operation[T]) {
    def when(condition: Boolean): Operation[Unit] =
      if (condition) operation.as(()) else Operations.ok(())

    def unless(condition: Boolean): Operation[Unit] =
      operation.when(!condition)

    def whenFailed(error: Error => Operation[T]): Operation[T] =
      Operations.whenFailed(operation, error)

    def |||(other: Operation[T]): Operation[T] =
      Operations.orElse(operation, other)

    def orElse(other: Operation[T]): Operation[T] =
      Operations.orElse(operation, other)
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
      asyncService.asyncDelay(a)

    def asyncFork[A](a: =>A)(implicit ec: ExecutionContext): Action[A] =
      AsyncFutureService.create.asyncFork(a)

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

    def fromError[A](error: ErrorEffect.Error): Action[A] =
      ErrorEffect.error(error)

    def orElse[A](action1: Action[A], action2: Action[A]): Action[A] =
      ErrorEffect.orElse(action1, action2)

    def whenFailed[A](action: Action[A], onError: Error => Action[A]): Action[A] =
      ErrorEffect.whenFailed(action, onError)

  }

  implicit class ioOperationToOption[T](operation: Operation[T]) {
    def runOption = runOperation(operation).toOption

    def toAction: Action[T] = operation
  }

  implicit def operationToAction[A](operation: Operation[A]): Action[A] =
    operation.into[ActionStack]

  object Operations {

    def log(m: String, doIt: Boolean = true): Operation[Unit] =
      ConsoleEffect.log(m, doIt)

    def logThrowable[R :_console](t: Throwable, doIt: Boolean = true): Eff[R, Unit] =
      ConsoleEffect.logThrowable(t, doIt)

    def logThrowable[R :_console](t: Throwable): Eff[R, Unit] =
      ConsoleEffect.logThrowable(t)

    def warn(m: String): Operation[Unit] =
      WarningsEffect.warn(m)

    def unit: Operation[Unit] =
      ok(())

    def ok[A](a: A): Operation[A] =
      ErrorEffect.ok(a)

    def protect[A](a: =>A): Operation[A] =
      SafeEffect.protect(a)

    def delayed[A](a: =>A): Operation[A] =
      ErrorEffect.ok(a)

    def fail[A](message: String): Operation[A] =
      ErrorEffect.fail[OperationStack, A](message)

    def exception[A](t: Throwable): Operation[A] =
      ErrorEffect.exception[OperationStack, A](t)

    def checkThat[A](a: =>A, condition: Boolean, failureMessage: String): Operation[A] =
      delayed(a).flatMap { value =>
        if (condition) delayed(value)
        else           fail(failureMessage)
      }

    def fromError[A](error: ErrorEffect.Error): Operation[A] =
      ErrorEffect.error(error)

    def orElse[A](operation1: Operation[A], operation2: Operation[A]): Operation[A] =
      ErrorEffect.orElse(operation1, operation2)

    def whenFailed[A](operation: Operation[A], onError: Error => Operation[A]): Operation[A] =
      ErrorEffect.whenFailed(operation, onError)

  }
}
