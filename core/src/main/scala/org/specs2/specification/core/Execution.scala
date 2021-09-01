package org.specs2
package specification
package core

import java.util.concurrent.*

import execute.*
import concurrent.ExecutionEnv

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import fp.{given, *}, syntax.*
import specification.process.Stats
import time.SimpleTimer, SimpleTimer.startSimpleTimer
import text.NotNullStrings.*
import control.*

import scala.util.control.NonFatal
import ResultLogicalCombinators.{given, *}
import Execution.{given, *}

/** Execution of a Fragment
  *
  *   - there can be none (for a piece of text)
  *   - the execution depends on the current Env. by default executions are created synchronously but can also be
  *     fork-off with Execution.withEnvAsync
  *   - it can have its own timeout (default is infinite)
  *   - once executed the result is kept
  *   - if mustJoin is true this means that all previous executions must be finished before this one can start
  *   - it has a condition deciding if the next execution can proceed or not depending on the current result
  *   - the result of a similar execution can be stored to decide if this one needs to be executed or not
  *   - it can store a continuation that will create more fragments, possibly containing more executions, based on the
  *     current result
  */
case class Execution(
    run: Option[Env => Future[() => Result]] = None,
    executing: Executing = NotExecuting,
    timeout: Option[FiniteDuration] = None,
    mustJoin: Boolean = false,
    nextMustStopIf: Result => Boolean = (r: Result) => false,
    previousResult: Option[Result] = None,
    finalResultMap: Option[Result => Result] = None,
    continuation: Option[FragmentsContinuation] = None
):

  lazy val executedResult: Action[ExecutedResult] =
    executing match {
      case NotExecuting =>
        Action.pure(ExecutedResult(Skipped(), new SimpleTimer))

      case Failed(t) =>
        Action.exception(t)

      case Started(f) =>
        Action.future(f).attempt.map {
          case Left(t) =>
            val r = Error(t)
            ExecutedResult(finalResultMap.map(_(r)).getOrElse(r), new SimpleTimer)

          case Right((r, timer)) =>
            ExecutedResult(finalResultMap.map(_(r)).getOrElse(r), timer)
        }
    }

  lazy val executionResult: Action[Result] =
    executedResult.map(_.result)

  private def futureResult(env: Env): Option[Future[(Result, SimpleTimer)]] =
    executing match
      case Failed(t) =>
        Some(Future.successful((Error(t), new SimpleTimer)))

      case Started(result) =>
        Some(result)

      case NotExecuting =>
        Some(Future.successful((Success(), new SimpleTimer)))

  /** methods to set the execution */

  def join = copy(mustJoin = true)

  def stopNextIf(r: Result): Execution =
    stopNextIf((r1: Result) => r1.status == r.status)

  def stopNextIf(f: Result => Boolean): Execution =
    copy(nextMustStopIf = f)

  def skip = setResult(Skipped())

  def setTimeout(timeout: FiniteDuration): Execution =
    copy(timeout = Some(timeout))

  def updateRun(newRun: (Env => Future[() => Result]) => (Env => Future[() => Result])) =
    copy(run = run.map(r => newRun(r)))

  def updateResult(newResult: (=>Result) => Result): Execution =
    updateRun(f => e => f(e).map(r => () => newResult(r()))(e.executionContext))

  /** map a result - the passed function can potentially throw an exception */
  def mapResult(f: Result => Result): Execution =
    updateResult(r => f(r))

  /** modify the final result - the passed function cannot throw an exception! */
  def mapFinalResult(f: Result => Result): Execution =
    copy(finalResultMap = Some(f))

  /** force a message */
  def updateMessage(f: String => String): Execution =
    mapResult(_.updateMessage(f))

  def setPreviousResult(r: Option[Result]): Execution =
    copy(previousResult = r)

  def was(statusCheck: String => Boolean): Boolean =
    previousResult.exists(r => statusCheck(r.status))

  /** run the execution */
  def startExecution(env: Env): Execution =
    run match
      case Some(r) =>
        given ec: ExecutionContext = env.specs2ExecutionContext
        val to = timeout.orElse(env.arguments.timeout)

        try
          // this sets any custom classloader, like the one passed from SBT
          // as the context classloader this thread
          env.setContextClassLoader()
          val timer = startSimpleTimer
          val timedFuture = Action.future {
            r(env).flatMap { action =>
              try Future.successful((action(), timer.stop))
              catch { case t: Throwable => Future.failed(t) }
            }
          }

          val future = timedFuture.runFuture(env.executionEnv, to).recoverWith {
            // this exception is thrown if the `action()` code above throws an exception
            case e: ExecutionException =>
              if (NonFatal(e.getCause))
                Future.successful((ResultExecution.handleExceptionsPurely(e.getCause), timer.stop))
              else
                Future.failed(FatalExecution(e.getCause))

            case NonFatal(e) =>
              // Future execution could still throw FailureExceptions or TimeoutExceptions
              // which can only be recovered here
              Future.successful((ResultExecution.handleExceptionsPurely(e), timer.stop))
          }

          setExecuting(future).copy(timeout = to)
        catch { case t: Throwable => setFatal(t) }
      case _ => this

  /** start this execution when the other one is finished */
  def startAfter(other: Execution)(env: Env): Execution =
    startAfter(List(other))(env)

  /** start this execution when the other ones are finished */
  def startAfter(others: List[Execution])(env: Env): Execution =
    val arguments = env.arguments
    val timer = startSimpleTimer

    val started: Action[(Result, SimpleTimer)] =
      others.map(_.executionResult).sequence.flatMap { results =>
        results.find(FatalExecution.isFatalResult) match
          // if a previous fragment was fatal, we skip the current one
          case Some(_) =>
            Action.pure((Skipped(): Result, timer.stop))

          case _ =>
            // if a previous result indicates that we should stop
            results.find { result =>
              arguments.stopOnFail && result.isFailure ||
              arguments.stopOnError && result.isError ||
              arguments.stopOnIssue && result.isIssue ||
              arguments.stopOnSkip && result.isSkipped ||
              nextMustStopIf(result)
            } match
              case Some(r) =>
                // if this execution is a step we still execute it
                // to allow for clean up actions
                if mustJoin then
                  startExecution(env).executionResult
                    .map(_ => (Error(FatalExecution(new Exception("stopped"))), timer.stop))
                // otherwise we skip
                else Action.pure((Skipped(): Result, timer.stop))

              // if everything is fine we run this current execution
              case _ =>
                startExecution(env).executionResult.map(r => (r, timer.stop))
      }
    copy(executing = Started(started.runFuture(env.executionEnv)))

  def setErrorAsFatal: Execution =
    updateResult { r =>
      try
        r match
          case Error(m, t) =>
            Error(m, FatalExecution(t))
          case other =>
            other
      catch {
        case t: Throwable =>
          Error(t.getMessage, FatalExecution(t))
      }
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
    Execution.withEnvFlatten { (env: Env) =>
      given ec: ExecutionContext = env.executionContext

      lazy val runs: List[Future[Result]] =
        executions.flatMap(_.futureResult(env).map(_.map(_._1)))

      lazy val before: Future[Result] =
        if sequential then runs.foldLeftM(Success(): Result)((res, cur) => cur.map(r => res and r))
        else Future.sequence(runs).map(_.suml)

      executing match
        case NotExecuting =>
          run match
            case Some(_) =>
              updateRun { r => (env: Env) =>
                before.flatMap { rs =>
                  if checkResult then
                    if rs.isSuccess then r(env)
                    else Future.successful(() => rs)
                  else r(env)
                }
              }
            case _ =>
              this

        case Failed(_) =>
          this

        case Started(f) =>
          val future = before.flatMap { rs =>
            if checkResult then
              if rs.isSuccess then f
              else Future.successful((rs, new SimpleTimer))
            else f
          }

          setExecuting(future)
    }

  /** @return true if something can be run */
  def isExecutable = run.isDefined

  /** @return set an execution result */
  def setResult(r: =>Result) =
    try copy(executing = executing.setResult(r))
    catch { case NonFatal(t) => copy(executing = Failed(t)) }

  /** @return set an execution result being computed */
  def setExecuting(r: Future[(Result, SimpleTimer)]): Execution =
    copy(executing = Started(r))

  /** @return set a fatal execution error */
  def setFatal(f: Throwable) =
    copy(executing = Failed(FatalExecution(f)))

  override def toString =
    "Execution(" +
      (if run.isDefined then "executable" else "no run") +
      previousResult.fold("")(", previous " + _) +
      ")"

  override def equals(a: Any) = a.asInstanceOf[Matchable] match
    case other: Execution =>
      other.run.isDefined == run.isDefined &&
        other.timeout == timeout &&
        other.mustJoin == mustJoin

    case _ => false

  override def hashCode =
    run.hashCode +
      executing.hashCode +
      timeout.hashCode +
      mustJoin.hashCode

trait Executing:
  def setResult(r: =>Result): Executing

case object NotExecuting extends Executing:
  def setResult(r: =>Result): Executing =
    val timer = startSimpleTimer
    Started(Future.successful((r, timer.stop)))

case class Failed(failure: Throwable) extends Executing:
  def setResult(r: =>Result): Executing =
    val timer = startSimpleTimer
    Started(Future.successful((r, timer.stop)))

case class Started(future: Future[(Result, SimpleTimer)]) extends Executing:
  def setResult(r: =>Result): Executing =
    val timer = startSimpleTimer
    Started(Future.successful((r, timer.stop)))

object Execution:

  /** create an execution with a Continuation */
  def apply[T: AsResult](r: =>T, continuation: FragmentsContinuation) =
    new Execution(
      run = Some((env: Env) => Future.successful(() => AsResult.safely(r))),
      continuation = Some(continuation)
    )

  /** create an execution returning a specific result */
  def result[T: AsResult](r: =>T): Execution =
    withEnv(_ => AsResult.safely(r))

  /** create an execution using the Env, synchronously by default */
  def withEnv[T: AsResult](f: Env => T): Execution =
    withEnvSync(f)

  /** create an execution using the Env and Flatten the execution */
  def withEnvFlatten(f: Env => Execution): Execution =
    Execution(Some { (env: Env) =>
      given ec: ExecutionContext = env.executionContext
      Future { () =>
        f(env).startExecution(env).executionResult.runFuture(env.executionEnv).map(r => () => r)
      }.flatMap(future => future())
    })

  /** create an execution using the Env */
  def withEnvSync[T: AsResult](f: Env => T): Execution =
    Execution(Some((env: Env) => Future.successful(() => AsResult.safely(f(env)))))

  /** create an execution with a future */
  def future[T: AsResult](f: =>Future[T]): Execution =
    withEnvAsync(_ => f)

  /** create an execution using the Env */
  def withEnvAsync[T: AsResult](f: Env => Future[T]): Execution =
    Execution(Some((env: Env) => f(env).map(r => () => AsResult.safely(r))(env.executionContext)))

  /** create an execution using the execution environment */
  def withExecutionEnv[T: AsResult](f: ExecutionEnv => T) =
    withEnv((env: Env) => f(env.executionEnv))

  /** create an execution using the execution context */
  def withExecutionContext[T: AsResult](f: ExecutionContext => T) =
    withEnv((env: Env) => f(env.executionContext))

  /** create an execution which will not execute but directly return a value */
  def executed[T: AsResult](r: T): Execution =
    lazy val f = Future.successful((AsResult.safely(r), new SimpleTimer))
    Execution(
      run = Some((e: Env) => f.map(res => () => res._1)(e.executionContext)),
      executing = Started(f)
    )

  given Show[Execution] with
    def show(e: Execution): String =
      e.executing match
        case NotExecuting => "no execution"
        case _            => "executing"

  /** nothing to execute */
  val NoExecution = Execution(run = None)

  /** insert the specification statistics for a given specification */
  def specificationStats(specClassName: String): Execution =
    withEnv((env: Env) => getStatistics(env, specClassName))

  /** get the execution statistics of a specification: a Decorated result */
  def getStatistics(env: Env, specClassName: String): Result =
    AsResult.safely(env.statisticsRepository.getStatisticsOr(specClassName, Stats.empty).map { s =>
      if s.examples == 0 then Pending(" "): Result // use a space to avoid PENDING to be appended after the spec name
      else DecoratedResult(s.copy(specs = s.specs + 1), s.result): Result
    })

  given Monoid[Option[FiniteDuration]] with
    val zero: Option[FiniteDuration] =
      None

    def append(f1: Option[FiniteDuration], f2: =>Option[FiniteDuration]): Option[FiniteDuration] =
      (f1, f2) match
        case (Some(t1), Some(t2)) => Some(t1 min t2)
        case (Some(t1), None)     => Some(t1)
        case (None, Some(t2))     => Some(t2)
        case _                    => None

  given AsExecution[Execution] with
    def execute(r: =>Execution): Execution = Execution.withEnvFlatten(_ => r)

  implicit def asExecutionToExecution[T: AsExecution](t: =>T): Execution =
    AsExecution[T].execute(t)

case class FatalExecution(t: Throwable) extends Exception(t):
  def toError: Result =
    Error("Fatal execution error, caused by " + t.getMessage.notNull, t)

object FatalExecution:

  def isFatalResult(r: Result): Boolean =
    r match
      case Error(_, t: FatalExecution) => true
      case _                           => false

case class ExecutedResult(result: Result, timer: SimpleTimer)
