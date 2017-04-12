package org.specs2.control

import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.eff.ConsoleEffect.Console
import org.specs2.control.eff.ErrorEffect.{Error, ErrorOrOk, exception}
import org.specs2.control.eff.{Eff, Fx, Fx1, TimedFuture}
import org.specs2.control.eff.syntax.safe._
import org.specs2.control.eff.syntax.error._
import org.specs2.control.eff.syntax.console._
import org.specs2.control.eff.syntax.warnings._
import org.specs2.control.eff.syntax.future._
import org.specs2.control.eff.syntax.eff._
import org.specs2.control.eff.WarningsEffect.Warnings
import org.specs2.execute.{AsResult, Result}
import org.specs2.fp.Monoid

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

trait ExecuteActions {

  def executeAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): (Error Either A, List[String]) = {
    implicit val es = ee.executorServices
    type S = Fx.append[Fx.fx2[TimedFuture, ErrorOrOk], Fx.fx2[Console, Warnings]]

    Await.result(action.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
      runError.runConsoleToPrinter(printer).runWarnings.into[Fx1[TimedFuture]].runAsync, Duration.Inf)
  }

  def executeActionFuture[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[(Error Either A, List[String])] = {
    implicit val es = ee.executorServices
    type S = Fx.append[Fx.fx2[TimedFuture, ErrorOrOk], Fx.fx2[Console, Warnings]]

    action.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
      runError.runConsoleToPrinter(printer).runWarnings.into[Fx1[TimedFuture]].runAsync
  }

  def runActionFuture[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[A] = {
    implicit val ec = ee.executionContext
    implicit val es = ee.executorServices

    action.runError.runConsoleToPrinter(printer).discardWarnings.execSafe.runAsync.flatMap {
      case Left(t)               => Future.failed(t)
      case Right(Left(Left(t)))  => Future.failed(t)
      case Right(Left(Right(s))) => Future.failed(new Exception(s))
      case Right(Right(a))       => Future.successful(a)
    }
  }

  def runAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Error Either A =
    attemptExecuteAction(action, printer)(ee).fold(
      t => Left(Left(t)),
      other => other._1)

  def attemptExecuteAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Throwable Either (Error Either A, List[String]) =
    try {
      implicit val es = ee.executorServices
      Await.result(action.runError.runConsoleToPrinter(printer).runWarnings.execSafe.runAsync, Duration.Inf)
    }
    catch { case NonFatal(t) => Left(t) }

  /**
   * This implicit allows an Action[result] to be used inside an example.
   *
   * For example to read a database.
   */
  implicit def actionAsResult[T](implicit r: AsResult[T], ee: ExecutionEnv): AsResult[Action[T]] = new AsResult[Action[T]] {
    def asResult(action: =>Action[T]): Result =
      runAction(action)(ee).fold(
        err => err.fold(t => org.specs2.execute.Error(t), f => org.specs2.execute.Failure(f)),
        ok => AsResult(ok)
      )
  }

  implicit class ActionRunOps[T](action: Action[T]) {
    def run(ee: ExecutionEnv)(implicit m: Monoid[T]): T =
      action.runOption(ee).getOrElse(m.zero)

    def runOption(ee: ExecutionEnv): Option[T] =
      runAction(action, println)(ee) match {
        case Right(a) => Option(a)
        case Left(t) => println("error while interpreting an action "+t.fold(Throwables.render, f => f)); None
      }
  }


}

object ExecuteActions extends ExecuteActions
