package org.specs2

import control.eff._
import all._
import syntax.all._

import scala.concurrent.duration._
import org.specs2.fp._
import org.specs2.fp.syntax._
import org.specs2.execute.{AsResult, Result}
import ErrorEffect.{Error, ErrorOrOk, exception, fail}
import ConsoleEffect._
import WarningsEffect._
import org.specs2.control.producer._
import org.specs2.concurrent._
import scala.concurrent._
import FutureEffect._

package object control {

  /**
   * Actions logging
   */
  type Logger = String => Unit
  lazy val noLogging = (s: String) => ()
  lazy val consoleLogging = (s: String) => println(s)

  sealed trait Message
  case object NoMessage extends Message
  case class ConsoleMessage(m: String) extends Message
  case class WarningMessage(m: String) extends Message

  case class Labelled[A](a: A, message: Message = Vector[Message] = Vector.empty)

  trait ExecutionIssue
  case class ThrowableIssue(t: Throwable) extends ExecutionIssue
  case class FailureIssue(t: String) extends ExecutionIssue
  case class FinalizationIssue(t: Throwable) extends ExecutionIssue

  type Execute[A] = Either[ExecutionIssue, Labelled[A]]

  case class Action1[A](runNow: ExecutorServices => Future[Execute[A]], timeout: Option[FiniteDuration] = None)

  object Action1 {
    def pure[A](a: =>A): Action1[A] =
      Action1Monad.point(a)

    def unit: Action1[Unit] =
      pure(())

    def protect[A](a: =>A): Action1[A] =
      Action1Monad.point(a)

    implicit val Action1Monad: Monad[Action1[?]] = new Monad[Action1[?]] {
      def point[A](a: =>A): Action1[A] =
        Action1(_ => Future.successful(Right(Labelled(a))))

      def bind[A, B](fa: Action1[A])(f: A => Action1[B]): Action1[B] =
        Action1[B](es => fa.runNow(es).flatMap {
          case Left(e) =>
          case Right(Labelled(a, message)) =>
            val (b, f(a).runNow(es))(es.executionContext)
        }

      def ap[A, B](fa: =>Action1[A])(ff: =>Action1[A => B]): Action1[B] = {
        val newRunNow= { es: ExecutorServices =>
          implicit val ec: ExecutionContext = es.executionContext

          Action1(ff.runNow(es)).flatMap(identity).flatMap { f =>
            Action1(fa.runNow(es)).flatMap(identity).map(f)
          }
        }
        Action1(newRunNow)
      }

      def tailrecM[A, B](a: A)(f: A => Action1[Either[A, B]]): Action1[B] =
        Action1[B] { es =>
          def loop(va: A): Future[B] = f(va).runNow(es).flatMap {
            case Left(na) => loop(na)
            case Right(nb) => Future.successful(nb)
          }(es.executionContext)
          loop(a)
        }

      def toString = "Monad[Action1]"
    }


  }
  case class Operation1[A](operation: Execute[A])

  type ActionStack = Fx.fx5[TimedFuture, ErrorOrOk, Console, Warnings, Safe]
  type OperationStack = Fx.fx4[ErrorOrOk, Console, Warnings, Safe]

  type Action[A] = Eff[ActionStack, A]
  type Operation[A] = Eff[OperationStack, A]

  type AsyncStream[A] = Producer[ActionStack, A]
  type AsyncStream1[A] = Producer1[A]

  type AsyncTransducer[A, B] = Transducer[ActionStack, A, B]

  type AsyncFold[A, B] = origami.Fold[Action, A, B]
  type AsyncSink[A] = origami.Fold[Action, A, Unit]

  implicit val idToAction: NaturalTransformation[Id, Action] =
    NaturalTransformation.naturalId[Action]

  def emitAsync[A](as: A*): AsyncStream[A] =
    producer.producers.emitSeq(as)

  def emitAsyncDelayed[A](a: A): AsyncStream[A] =
    producer.producers.eval(futureDelay(a))

  /**
   * warn the user about something that is probably wrong on his side,
   * this is not a specs2 bug, then fail to stop all further computations
   */
  def warnAndFail[R, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: ErrorOrOk <= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def runOperation[A](operation: Operation[A], printer: String => Unit = s => ()): Error Either A =
    attemptExecuteOperation(operation, printer).fold(
      t => Left(Left(t)),
      other => other._1)

  def executeOperation[A](operation: Operation[A], printer: String => Unit = s => ()): (Error Either A, List[String]) = {
    type S = Fx.fx3[ErrorOrOk, Console, Warnings]

    operation.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
      runError.runConsoleToPrinter(printer).runWarnings.run
  }

  def attemptExecuteOperation[A](operation: Operation[A], printer: String => Unit = s => ()): Throwable Either (Error Either A, List[String]) =
    operation.runError.runConsoleToPrinter(printer).runWarnings.execSafe.run

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

    def asyncDelayAction[A](a: =>A): Action[A] =
      futureDelay[ActionStack, A](a)

    def asyncFuture[A](fa: =>Future[A], timeout: Option[FiniteDuration] = None): Action[A] =
      futureDefer[ActionStack, A](fa, timeout)

    def timedFuture[A](fa: TimedFuture[A]): Action[A] =
      send[TimedFuture, ActionStack, A](fa)

    def asyncForkAction[A](a: =>A, ec: ExecutionContext, timeout: Option[FiniteDuration] = None): Action[A] =
      futureFork[ActionStack, A](a, ec, timeout)

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

  implicit def operationToActionNat[A]: Operation ~> Action = new (Operation ~> Action) {
    def apply[X](operation: Operation[X]): Action[X] = operation.into[ActionStack]
  }

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

    def attempt[A](a: Operation[A]): Operation[Throwable Either A] =
      SafeEffect.attempt(a)

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
