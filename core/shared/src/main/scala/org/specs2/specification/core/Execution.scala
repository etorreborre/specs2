package org.specs2
package specification
package core

import java.util.concurrent._
import execute._
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import fp.{Monoid, Show}
import fp.syntax._
import specification.process.Stats
import time.SimpleTimer
import text.NotNullStrings._
import control._

import scala.util.control.NonFatal
import ResultLogicalCombinators._
import org.specs2.control.eff.TimedFuture
import Execution._

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
case class Execution(run:            Option[Env => Future[() => Result]]     = None,
                     executing:      Option[Throwable Either Future[Result]] = None,
                     timeout:        Option[FiniteDuration]                  = None,
                     mustJoin:       Boolean                                 = false,
                     nextMustStopIf: Result => Boolean                       = (r: Result) => false,
                     isolable:       Boolean                                 = true,
                     previousResult: Option[Result]                          = None,
                     timer:          SimpleTimer                             = new SimpleTimer,
                     continuation:   Option[FragmentsContinuation]           = None) {

  lazy val executedResult: TimedFuture[ExecutedResult] =
    executing match {
      case None =>
        TimedFuture.successful(ExecutedResult(Skipped(), timer))

      case Some(Left(t)) =>
        TimedFuture.failed(t)

      case Some(Right(f)) =>
        TimedFuture.future(f).map(r => ExecutedResult(r, timer.stop))
    }

  lazy val executionResult: TimedFuture[Result] =
    executedResult.attempt.map {
      case Left(t: TimeoutException) =>
        timeout match {
          case Some(to) => Skipped(s"timed out after $to", t.getMessage)
          case None => Error(t)
        }
      case Left(t) => Error(t)
      case Right(ExecutedResult(r, _)) => r
    }

  private def futureResult(env: Env): Option[Future[Result]] =
    executing match {
      case Some(Left(t))       => Some(Future.successful(Error(t)))
      case Some(Right(result)) => Some(result)
      case None                => run.map(_(env).map(_())(env.executionContext))
    }

  /** methods to set the execution */

  def join = copy(mustJoin = true)

  def stopNextIf(r: Result): Execution =
    stopNextIf((r1: Result) => r1.status == r.status)

  def stopNextIf(f: Result => Boolean): Execution =
    copy(nextMustStopIf = f)

  def skip = setResult(Skipped())

  def makeGlobal: Execution =
    makeGlobal(when = true)
  def makeGlobal(when: Boolean): Execution =
    copy(isolable = !when)

  def setTimeout(timeout: FiniteDuration): Execution =
    copy(timeout = Some(timeout))

  def updateRun(newRun: (Env => Future[() => Result]) => (Env => Future[() => Result])) =
    copy(run = run.map(r => newRun(r)))

  def updateResult(newResult: (=>Result) => Result): Execution =
    updateRun(f => e => f(e).map(r => () => newResult(r()))(e.executionContext))

  /** force a result */
  def mapResult(f: Result => Result): Execution =
    updateResult(r => f(r))

  /** force a message */
  def mapMessage(f: String => String): Execution =
    mapResult(_.mapMessage(f))

  def setPreviousResult(r: Option[Result]): Execution =
    copy(previousResult = r)

  def was(statusCheck: String => Boolean): Boolean =
    previousResult.exists(r => statusCheck(r.status))

  /** run the execution */
  def startExecution(env: Env): Execution =
    run match {
      case None => this

      case Some(r) =>
        val to = env.timeout |+| timeout
        try {
          implicit val ec = env.executionContext
          env.setContextClassLoader()

          val future = TimedFuture(es => r(env).map(_()), to).runNow(env.executorServices)
          setExecuting(future).copy(timeout = to).startTimer
        }
        catch { case t: Throwable => setFatal(t).stopTimer }
    }

  /** start this execution when the other one is finished */
  def startAfter(other: Execution)(env: Env): Execution =
    startAfter(List(other))(env)

  /** start this execution when the other ones are finished */
  def startAfter(others: List[Execution])(env: Env): Execution = {
    val arguments = env.arguments

    val started: TimedFuture[Result] =
      others.map(_.executionResult).sequence.flatMap { results =>
        results.find(FatalExecution.isFatalResult) match {
          // if a previous fragment was fatal, we skip the current one
          case Some(_) =>
            TimedFuture.successful(Skipped(): Result)

          case None =>
            // if a previous result indicates that we should stop
            results.find { result =>
              arguments.stopOnFail  && result.isFailure  ||
              arguments.stopOnError && result.isError    ||
              arguments.stopOnIssue && result.isIssue    ||
              arguments.stopOnSkip  && result.isSkipped  ||
              nextMustStopIf(result)
            } match {
              case Some(r) =>
                // if this execution is a step we still execute it
                // to allow for clean up actions
                if (mustJoin)
                  startExecution(env).executionResult.map(_ => Error(FatalExecution(new Exception("stopped"))))
                // otherwise we skip
                else
                  TimedFuture.successful(Skipped(): Result)

              // if everything is fine we run this current execution
              case None =>
                startExecution(env).executionResult
            }
      }
    }
    copy(executing = Some(Right(started.runNow(env.executorServices))))
  }

  def setErrorAsFatal: Execution =
    updateResult { r =>
      try r match {
        case Error(m, t) =>
          Error(m, FatalExecution(t))
        case other => other
      } catch { case t: Throwable => Error(t.getMessage, FatalExecution(t)) }
    }

  /** run this execution after the previous executions are finished */
  def after(executions: List[Execution]): Execution =
    afterExecutions(executions, sequential = false, checkResult = false)

  /** run this execution after the executions and only if they are successful */
  def afterSuccessful(executions: List[Execution]): Execution =
    afterExecutions(executions, sequential = false, checkResult = true)

  /** run this execution after the other executions have been sequentially executed and only if they are successful */
  def afterSuccessfulSequential(executions: List[Execution]): Execution =
    afterExecutions(executions, sequential = true, checkResult = true)

  /** run this execution after the other executions have been sequentially executed */
  def afterSequential(executions: List[Execution]): Execution =
    afterExecutions(executions, sequential = true, checkResult = false)

  /** run this execution after the previous executions are finished */
  private def afterExecutions(executions: List[Execution], sequential: Boolean, checkResult: Boolean): Execution =
    Execution.withEnvFlatten { env: Env =>
      implicit val ec = env.executionContext

      lazy val runs: List[Future[Result]] =
        executions.flatMap(_.futureResult(env))

      lazy val before: Future[Result] =
        if (sequential) runs.foldLeftM[Future, Result](Success())((res, cur) => cur.map(r => res and r))
        else            Future.sequence(runs).map(_.suml)

      executing match {
        case None =>
          run match {
            case None => this
            case _ =>
              updateRun { r => (env: Env) =>
                before.flatMap { rs =>
                  if (checkResult) {
                    if (rs.isSuccess) r(env)
                    else Future.successful(() => rs)
                  } else r(env)
                }
              }
          }
        case Some(Left(t)) =>
          this

        case Some(Right(f)) =>

          val future = before.flatMap { rs =>
            if (checkResult) {
              if (rs.isSuccess) f
              else              Future.successful(rs)
            } else f
          }

          setExecuting(future)
      }
  }

  /** @return true if something can be run */
  def isExecutable = run.isDefined

  /** @return set an execution result */
  def setResult(r: =>Result) =
    try copy(executing = Some(Right(Future.successful(r))))
    catch { case NonFatal(t) => copy(executing = Some(Left(t))) }

  /** @return set an execution result being computed */
  def setExecuting(r: Future[Result]): Execution =
    copy(executing = Option(Right(r)))

  /** @return set a fatal execution error */
  def setFatal(f: Throwable) =
    copy(executing = Some(Left(FatalExecution(f))))

  /** @return start the timer */
  def startTimer = copy(timer = timer.start)

  /** @return stop the timer */
  def stopTimer = copy(timer = timer.stop)

  override def toString =
    "Execution("+
      (if (run.isDefined) "executable" else "no run")+
      (if (!isolable) ", global" else "") +
      previousResult.fold("")(", previous " + _) +
     ")"

  override def equals(a: Any) = a match {
    case other: Execution =>
      other.run.isDefined == run.isDefined &&
      other.timeout == timeout &&
      other.mustJoin == mustJoin &&
      other.isolable == isolable

    case _ => false
  }

  override def hashCode =
    run.hashCode +
    executing.hashCode +
    timeout.hashCode +
    mustJoin.hashCode +
    isolable.hashCode
}

object Execution {

  /** create an execution with a Continuation */
  def apply[T : AsResult](r: =>T, continuation: FragmentsContinuation) =
    new Execution(run = Some((env: Env) => Future.successful(() => AsResult.safely(r))), continuation = Some(continuation))

  /** create an execution returning a specific result */
  def result[T : AsResult](r: =>T): Execution =
    withEnv(_ => AsResult.safely(r))

  /** create an execution using the Env */
  def withEnv[T : AsResult](f: Env => T): Execution =
    Execution(Some((env: Env) => Future(() => AsResult.safely(f(env)))(env.executionContext)))

  /** create an execution using the Env and Flatten the execution */
  def withEnvFlatten(f: Env => Execution): Execution =
    Execution(Some { env: Env =>
      implicit val ec = env.executionContext
      Future {
        () =>
        f(env).startExecution(env).executionResult.runNow(env.executorServices).map(r => () => r)
      }.flatMap(future => future())
    })

  /** create an execution using the Env */
  def withEnvSync[T : AsResult](f: Env => T): Execution =
    Execution(Some((env: Env) => Future.successful(() => AsResult.safely(f(env)))))

  /** create an execution using the Env */
  def withEnvAsync[T : AsResult](f: Env => Future[T]): Execution =
    Execution(Some((env: Env) => f(env).map(r => () => AsResult.safely(r))(env.executionContext)))

  /** create an execution using the execution environment */
  def withExecutionEnv[T : AsResult](f: ExecutionEnv => T) =
    withEnv((env: Env) => f(env.executionEnv))

  /** create an execution using the execution context */
  def withExecutionContext[T : AsResult](f: ExecutionContext => T) =
    withEnv((env: Env) => f(env.executionContext))

  /** create an execution which will not execute but directly return a value */
  def executed[T : AsResult](r: T): Execution = {
    lazy val f = Future.successful(AsResult.safely(r))
    Execution(
      run = Some((e: Env) => f.map(res => () => res)(e.executionContext)),
      executing = Some(Right(f))
    )
  }

  def sequence(executions: List[Execution])(env: Env): Execution = withEnvAsync { env =>
    implicit val ec = env.executionContext
    val executings = executions.flatMap(_.executing.flatMap(_.toOption))
    Future.sequence(executings).map(_.suml)
  }.startExecution(env)

  implicit def showInstance: Show[Execution] = new Show[Execution] {
    def show(e: Execution): String =
      e.executing match {
        case None => "no execution"
        case Some(_) => "executing"
      }
  }

  /** nothing to execute */
  val NoExecution = Execution(run = None)

  /** insert the specification statistics for a given specification */
  def specificationStats(specClassName: String): Execution =
    withEnv((env: Env) => getStatistics(env, specClassName))

  /** get the execution statistics of a specification as a Decorated result */
  def getStatistics(env: Env, specClassName: String): Result =
    AsResult.safely(env.statisticsRepository.getStatisticsOr(specClassName, Stats.empty).map { s =>
      if (s.examples == 0) Pending(" "): Result // use a space to avoid PENDING to be appended after the spec name
      else                 DecoratedResult(s.copy(specs = s.specs + 1), s.result): Result
    })

  implicit def finiteDurationMonoid: Monoid[Option[FiniteDuration]] = new Monoid[Option[FiniteDuration]] {
    val zero: Option[FiniteDuration] =
      None

    def append(f1: Option[FiniteDuration], f2: =>Option[FiniteDuration]): Option[FiniteDuration] =
      (f1, f2) match {
        case (Some(t1), Some(t2)) => Some(t1 min t2)
        case (Some(t1), None)     => Some(t1)
        case (None,     Some(t2)) => Some(t2)
        case _                    => None
      }

  }

}

case class FatalExecution(t: Throwable) extends Exception(t) {
  def toError: Result =
    Error("Fatal execution error, caused by " + t.getMessage.notNull, t)
}

object FatalExecution {

  def isFatalResult(r: Result): Boolean =
    r match {
      case Error(_, t: FatalExecution) => true
      case _ => false
    }

}

case class ExecutedResult(result: Result, timer: SimpleTimer)
