package org.specs2
package specification
package core

import java.util.concurrent.ExecutorService

import execute._
import scala.concurrent.duration._
import scalaz.Show
import scalaz.std.anyVal._
import specification.process.Stats
import time.SimpleTimer
import control._

case class Execution(run:            Option[Env => Result],
                     executed:       Option[Result]                = None,
                     duration:       Duration                      = Duration(1, DAYS),
                     mustJoin:       Boolean                       = false,
                     nextMustStopIf: Result => Boolean             = (r: Result) => false,
                     isolable:       Boolean                       = true,
                     previousResult: Option[Result]                = None,
                     executionTime:  SimpleTimer                   = new SimpleTimer,
                     continuation:   Option[FragmentsContinuation] = None) {

  lazy val executedResult = executed
  lazy val result = executedResult.getOrElse(org.specs2.execute.Success("no execution yet defined"))
  def isExecuted = executedResult.isDefined

  def execute(env: Env) = run.fold(this)(r => setResult(r(env)))

  def updateRun(newRun: (Env => Result) => (Env => Result)) = copy(run = run.map(r => newRun(r)))
  def setResult(r: =>Result) = copy(executed = Some(r))
  def join = copy(mustJoin = true)
  def stopNextIf(r: Result): Execution            = stopNextIf((r1: Result) => r1.status == r.status)
  def stopNextIf(f: Result => Boolean): Execution = copy(nextMustStopIf = f)
  def skip = setResult(Skipped())
  def makeGlobal: Execution = makeGlobal(when = true)
  def makeGlobal(when: Boolean): Execution = copy(isolable = !when)
  def isRunnable = run.isDefined

  def setPreviousResult(r: Option[Result]) = copy(previousResult = r)
  def was(statusCheck: String => Boolean) = previousResult.exists(r => statusCheck(r.status))

  def setExecutionTime(timer: SimpleTimer) = copy(executionTime = timer)
  def time = executionTime.time

  def mapResult(f: Result => Result) =
    updateRun(run => (env: Env) => f(run(env)))

  def mapMessage(f: String => String) =
    mapResult(_.mapMessage(f))

  override def toString =
    "Execution("+
      (if (run.isDefined) "executable" else "no run")+
      (if (!isolable) ", global" else "") +
      previousResult.fold("")(", previous " + _) +
     ")"

  override def equals(a: Any) = a match {
    case other: Execution =>
      other.run.isDefined == run.isDefined &&
      other.executed.isDefined == executed.isDefined &&
      other.duration == duration &&
      other.mustJoin == mustJoin &&
      other.isolable == isolable

    case _ => false
  }
}

object Execution {
  def apply[T : AsResult](r: =>T, continuation: FragmentsContinuation) =
    new Execution(run = Some((env: Env) => AsResult(r)), continuation = Some(continuation))

  def result[T : AsResult](r: =>T)       = withEnv(_ => AsResult(r))
  def withEnv[T : AsResult](f: Env => T) = Execution(Some((env: Env) => AsResult(f(env))))
  def withExecutorService[T : AsResult](f: ExecutorService => T) =
    withEnv((env: Env) => f(env.executorService))

  def executed[T : AsResult](r: T): Execution = {
    lazy val asResult = AsResult(r)
    Execution(run = Some((env: Env) => asResult), executed = Some(asResult))
  }

  implicit def showInstance: Show[Execution] = new Show[Execution] {
    override def shows(e: Execution): String =
      s"${e.result.toString}"
  }

  val NoExecution = Execution(run = None)

  def SpecificationStats(specClassName: String) =
    withEnv((env: Env) => getStatistics(env, specClassName))

  def getStatistics(env: Env, specClassName: String): Result =
    AsResult(env.statisticsRepository.getStatisticsOr(specClassName, Stats()).map { s =>
      if (s.examples == 0) Pending(" ") // use a space to avoid PENDING to be appended after the spec name
      else                 DecoratedResult(s, s.result)
    })

}

