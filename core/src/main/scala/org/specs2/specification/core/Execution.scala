package org.specs2
package specification
package core

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeoutException}

import execute._
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scalaz.Show
import specification.process.Stats
import time.SimpleTimer
import text.NotNullStrings._
import control._

import scala.util.control.NonFatal

/**
 * Execution of a Fragment
 *
 *  - there can be none (for a piece of text)
 *  - the execution depends on the current Env
 *  - it can have its own timeout (default is infinite)
 *  - once executed the result is kept
 *  - if mustJoin is true this means that all previous executions must be finished before this one can start
 *  - it has a condition deciding if the next execution can proceed or not depending on the current result
 *  - if isolable is true this means that it should be executed in its own specification instance
 *  - the result of a similar execution can be stored to decide if this one needs to be executed or not
 *  - it stores its execution time
 *  - it can store a continuation that will create more fragments, possibly containing more executions, based on the
 *    current result
 */
case class Execution(run:            Option[Env => Future[Result]]           = None,
                     executing:      Option[Throwable Either Future[Result]] = None,
                     timeout:        Option[FiniteDuration]                  = None,
                     mustJoin:       Boolean                                 = false,
                     nextMustStopIf: Result => Boolean                       = (r: Result) => false,
                     isolable:       Boolean                                 = true,
                     previousResult: Option[Result]                          = None,
                     timer:          SimpleTimer                             = new SimpleTimer,
                     continuation:   Option[FragmentsContinuation]           = None) {

  lazy val executedResult: Option[Result] =
    executed match {
      case Left(_)  => None
      case Right(o) => o
    }

  lazy val executed: Throwable Either Option[Result] =
    executing match {
      case None =>
        Right(None)

      case Some(Left(t)) =>
        Left(t)

      case Some(Right(f)) =>
        try Right(Some(Await.result(f, timeout.getOrElse(Duration.Inf))))
        catch {
          case t: TimeoutException => Right(Option(Skipped(t.getMessage)))
          case NonFatal(t) => Left(t)
        }
    }

  lazy val executionTime: SimpleTimer =
    timer.stop

  /** if the execution hasn't been executed, the result is Success */
  lazy val result =
    executedResult.getOrElse(org.specs2.execute.Success("no execution yet defined"))

  lazy val executionFatalOrResult: Throwable Either Result =
    executed match {
      case Left(t)  => Left(t)
      case Right(r) => Right(r.getOrElse(org.specs2.execute.Success()))
    }


  def isExecuted = executedResult.isDefined

  /** methods to set the execution */

  def join = copy(mustJoin = true)

  def stopNextIf(r: Result): Execution            = stopNextIf((r1: Result) => r1.status == r.status)
  def stopNextIf(f: Result => Boolean): Execution = copy(nextMustStopIf = f)

  def skip = setResult(Skipped())

  def makeGlobal: Execution = makeGlobal(when = true)
  def makeGlobal(when: Boolean): Execution = copy(isolable = !when)

  def setTimeout(timeout: FiniteDuration) = copy(timeout = Some(timeout))

  def updateRun(newRun: (Env => Future[Result]) => (Env => Future[Result])) =
    copy(run = run.map(r => newRun(r)))

  def updateResult(newResult: (=>Result) => Result) =
    updateRun(f => e => f(e).map(r => newResult(r))(e.executionContext))

  /** force a result */
  def mapResult(f: Result => Result) = updateRun(run => (env: Env) => run(env).map(f)(env.executionContext))

  /** force a message */
  def mapMessage(f: String => String) = mapResult(_.mapMessage(f))

  def setPreviousResult(r: Option[Result]) = copy(previousResult = r)
  def was(statusCheck: String => Boolean) = previousResult.exists(r => statusCheck(r.status))

  /** run the execution */
  def startExecution(env: Env): Execution =
    run match {
      case None => this

      case Some(r) =>
        try   setExecuting(r(env)).startTimer
        catch { case NonFatal(t) => copy(executing = Some(Left(t))).stopTimer }
    }

  /** wait for this execution */
  lazy val finishExecution: Execution = {
    executedResult
    this
  }

  def execute(env: Env): Execution =
    startExecution(env).finishExecution

  /** @return true if something can be run */
  def isExecutable = run.isDefined

  /** @return set an execution result */
  def setResult(r: =>Result) =
    try copy(executing = Some(Right(Future.successful(r))))
    catch { case NonFatal(t) => copy(executing = Some(Left(t))) }

  /** @return set an execution result being computed */
  def setExecuting(r: Future[Result]) = copy(executing = Option(Right(r)))

  /** @return set a fatal execution error */
  def setFatal(f: Throwable) = copy(executing = Some(Left(FatalExecution(f))))

  /** @return start the timer */
  def startTimer = copy(timer = timer.start)

  /** @return stop the timer */
  def stopTimer = copy(timer = timer.stop)

  /** @return the execution time */
  def time = executionTime.time

  override def toString =
    "Execution("+
      (if (run.isDefined) "executable" else "no run")+
      (if (!isolable) ", global" else "") +
      previousResult.fold("")(", previous " + _) +
     ")"

  override def equals(a: Any) = a match {
    case other: Execution =>
      other.run.isDefined == run.isDefined &&
      other.executedResult.isDefined == executedResult.isDefined &&
      other.timeout == timeout &&
      other.mustJoin == mustJoin &&
      other.isolable == isolable

    case _ => false
  }

  override def hashCode =
    run.hashCode +
    executed.hashCode +
    timeout.hashCode +
    mustJoin.hashCode +
    isolable.hashCode
}

object Execution {

  /** create an execution with a Continuation */
  def apply[T : AsResult](r: =>T, continuation: FragmentsContinuation) =
    new Execution(run = Some((env: Env) => Future.successful(AsResult(r))), continuation = Some(continuation))

  def fatal(t: Throwable): Execution =
    new Execution(executing = Some(Left(FatalExecution(t))))

  /** create an execution returning a specific result */
  def result[T : AsResult](r: =>T) = withEnv(_ => AsResult(r))

  /** create an execution using the Env */
  def withEnv[T : AsResult](f: Env => T) = Execution(Some((env: Env) => Future(AsResult(f(env)))(env.executionContext)))

  /** create an execution using the executor service */
  def withExecutorService[T : AsResult](f: ExecutorService => T) =
    withEnv((env: Env) => f(env.executorService))

  /** create an execution using the scheduled executor service */
  def withScheduledExecutorService[T : AsResult](f: ScheduledExecutorService => T) =
    withEnv((env: Env) => f(env.scheduledExecutorService))

  /** create an execution using the execution environment */
  def withExecutionEnv[T : AsResult](f: ExecutionEnv => T) =
    withEnv((env: Env) => f(env.executionEnv))

  /** create an execution using the execution context */
  def withExecutionContext[T : AsResult](f: ExecutionContext => T) =
    withEnv((env: Env) => f(env.executionContext))

  /** create an execution which will not execute but directly return a value */
  def executed[T : AsResult](r: T): Execution = {
    lazy val asResult = AsResult(r)
    Execution(executing = Some(Right(Future.successful(asResult))))
  }

  implicit def showInstance: Show[Execution] = new Show[Execution] {
    override def shows(e: Execution): String =
      s"${e.result.toString}"
  }

  /** nothing to execute */
  val NoExecution = Execution(run = None)

  /** insert the specification statistics for a given specification */
  def specificationStats(specClassName: String): Execution =
    withEnv((env: Env) => getStatistics(env, specClassName))

  /** get the execution statistics of a specification as a Decorated result */
  def getStatistics(env: Env, specClassName: String): Result =
    AsResult(env.statisticsRepository.getStatisticsOr(specClassName, Stats.empty).map { s =>
      if (s.examples == 0) Pending(" "): Result // use a space to avoid PENDING to be appended after the spec name
      else                 DecoratedResult(s.copy(specs = s.specs + 1), s.result): Result
    })

}

case class FatalExecution(t: Throwable) extends Exception(t) {
  def toError: Result =
    Error("Fatal execution error, caused by "+t.getMessage.notNull, t)
}
