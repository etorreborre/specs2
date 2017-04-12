package org.specs2.control

import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.eff.ErrorEffect._
import org.specs2.control.eff.syntax.safe._
import org.specs2.control.eff.syntax.error._
import org.specs2.control.eff.syntax.console._
import org.specs2.control.eff.syntax.warnings._
import org.specs2.control.eff.syntax.future._
import org.specs2.execute.{AsResult, Result}
import org.specs2.fp.Monoid

import scala.concurrent._

trait ExecuteActions {

  def executeAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): (Error Either A, List[String]) =
    throw new Exception("executeAction not implemented")

  def executeActionFuture[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[(Error Either A, List[String])] = {
    implicit val es = ee.executorServices
    implicit val executionContext = ee.executionContext

    action.runError.runConsoleToPrinter(printer).execSafe.runWarnings.runAsync.map {
      case (Left(t), ws)  => (Left(Left(t)), ws)
      case (Right(e), ws) => (e, ws)
    }
  }

  def runActionFuture[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[A] = {
    implicit val executionContext = ee.executionContext

    executeActionFuture(action, printer)(ee).map(_._1).flatMap {
      case Left(Left(t))  => Future.failed(t)
      case Left(Right(s)) => Future.failed(new Exception(s))
      case Right(a)       => Future.successful(a)
    }
  }

  def runAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Error Either A =
    throw new Exception("runAction not implemented")

  def attemptExecuteAction[A](action: Action[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Throwable Either (Error Either A, List[String]) =
    throw new Exception("attemptExecuteAction not implemented")

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

    def runFuture(ee: ExecutionEnv): Future[T] =
      runActionFuture(action)(ee)

    def runOption(ee: ExecutionEnv): Option[T] =
      runAction(action, println)(ee) match {
        case Right(a) => Option(a)
        case Left(t) => println("error while interpreting an action "+t.fold(Throwables.render, f => f)); None
      }
  }


}

object ExecuteActions extends ExecuteActions

