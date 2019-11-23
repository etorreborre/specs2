package org.specs2.control

// import org.specs2.concurrent.ExecutionEnv
// import org.specs2.execute.{AsResult, Result}
// import org.specs2.fp.Monoid

// import scala.concurrent.{Await, Future}
// import scala.concurrent.duration.Duration
// import scala.util.control.NonFatal

trait ExecuteActions1 {

  /*
  trait ExecutionIssue extends Throwable
case class ThrowableIssue(t: Throwable) extends ExecutionIssue
case class FailureIssue(t: String) extends ExecutionIssue
case class FinalizationIssue(t: Throwable) extends ExecutionIssue

case class Action1[A](runNow: ExecutorServices => Future[Execute[A]], timeout: Option[FiniteDuration] = None, last: Vector[Action1[Unit]] = Vector.empty) {
  def addLast(action: Action1[Unit]): Action1[A] =
    copy(last = last :+ action)


    sealed trait Message
case object NoMessage extends Message
case class ConsoleMessage(m: String) extends Message
case class WarningMessage(m: String) extends Message

case class Labelled[A](a: A, messages: Vector[Message] = Vector.empty)

}

object Action1 {
  type Execute[A] = Either[ExecutionIssue, Labelled[A]]


  */


  // def executeActionFuture[A](action: Action1[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[(Error Either A, List[String])] = {
  //   implicit val es = ee.executorServices

  //   action.execSafe.flatMap(_.fold(t => exception[S, A](t), a => Eff.pure[S, A](a))).
  //     runError.runConsoleToPrinter(printer).runWarnings.into[Fx1[TimedFuture]].runAsync
  // }

  // def runActionFuture[A](action: Action1[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Future[A] = {
  //   implicit val ec = ee.executionContext
  //   implicit val es = ee.executorServices

  //   action.runError.runConsoleToPrinter(printer).discardWarnings.execSafe.runAsync.flatMap {
  //     case Left(t)               => Future.failed(t)
  //     case Right(Left(Left(t)))  => Future.failed(t)
  //     case Right(Left(Right(s))) => Future.failed(new Exception(s))
  //     case Right(Right(a))       => Future.successful(a)
  //   }
  // }

  // def runAction[A](action: Action1[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Error Either A =
  //   attemptExecuteAction(action, printer)(ee).fold(
  //     t => Left(Left(t)),
  //     other => other._1)

  // def attemptExecuteAction[A](action: Action1[A], printer: String => Unit = s => ())(ee: ExecutionEnv): Throwable Either (Error Either A, List[String]) =
  //   try {
  //     implicit val es = ee.executorServices
  //     Await.result(action.runError.runConsoleToPrinter(printer).runWarnings.execSafe.runAsync, Duration.Inf)
  //   }
  //   catch { case NonFatal(t) => Left(t) }

  // /**
  //  * This implicit allows an Action1[result] to be used inside an example.
  //  *
  //  * For example to read a database.
  //  */
  // implicit def actionAsResult[T](implicit r: AsResult[T], ee: ExecutionEnv): AsResult[Action1[T]] = new AsResult[Action1[T]] {
  //   def asResult(action: =>Action1[T]): Result =
  //     runAction(action)(ee).fold(
  //       err => err.fold(t => org.specs2.execute.Error(t), f => org.specs2.execute.Failure(f)),
  //       ok => AsResult(ok)
  //     )
  // }

  // implicit class ActionRunOps[T](action: Action1[T]) {
  //   def run(ee: ExecutionEnv)(implicit m: Monoid[T]): T =
  //     action.runOption(ee).getOrElse(m.zero)

  //   def runOption(ee: ExecutionEnv): Option[T] =
  //     runAction(action, println)(ee) match {
  //       case Right(a) => Option(a)
  //       case Left(t) => println("error while interpreting an action "+t.fold(Throwables.render, f => f)); None
  //     }

  //   def runEither(ee: ExecutionEnv): Either[Error, T] =
  //     runAction(action, println)(ee)
  // }


}

object ExecuteActions1 extends ExecuteActions1
