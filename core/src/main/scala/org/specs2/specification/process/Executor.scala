package org.specs2
package specification
package process

import org.specs2.control.Timer

import scalaz.stream.Process.{Env =>_,_}
import execute._
import scalaz.stream.{Process, process1}
import scalaz.{Success=>_, Failure=>_,_}, Scalaz._
import specification.core._
import Description._
import time.SimpleTimer
import scalaz.concurrent.{Future, Task}
import scala.concurrent.duration.Duration

/**
 * Functions for executing fragments.
 *
 * The default execution model executes all examples concurrently and uses steps as
 * "join" points
 *
 */
trait Executor {

  /**
   * execute fragments:
   *
   *  - filter the ones that the user wants to keep
   *  - sequence the execution so that only parts in between steps are executed concurrently
   */
  def execute(env: Env): Process[Task, Fragment] => Process[Task, Fragment] = { contents: Process[Task, Fragment] =>
    (contents |> sequencedExecution(env)).sequence(Runtime.getRuntime.availableProcessors).flatMap(executeOnline(env))
  }

  /** a Process1 to execute fragments as tasks */
  def executeTasks(env: Env): Process1[Fragment, Task[Fragment]] =
    sequencedExecution(env)

  /**
   * execute fragments, making sure that:
   *
   *  - "join" points are respected, i.e. when a Fragment is a join we must make sure that all previously
   *    executing fragments have finished their execution
   *
   *  - the fragments execute sequentially when args.sequential
   *
   *  - the execution stops if one fragment indicates that the result of the previous executions is not correct
   */
  def sequencedExecution(env: Env, barrier: Task[Result] = Task.now(Success()), mustStop: Boolean = false): Process1[Fragment, Task[Fragment]] =
    receive1 { fragment: Fragment =>
      val arguments = env.arguments

      // if we need to wait, we do, an get the result
      val barrierResult =
        if (fragment.execution.mustJoin) barrier.attemptRun.fold(t => Error(t), r => r)
        else                             Success()

      // depending on the result we decide if we should go on executing fragments
      val nextMustStop = mustStop ||
                         arguments.stopOnFail && barrierResult.isFailure ||
                         arguments.stopOnSkip && barrierResult.isSkipped ||
                         fragment.execution.nextMustStopIf(barrierResult)

      // if the previous fragments decided that we should stop the execution
      // skip the execution
      // otherwise execute synchronously or asynchronously

      // make sure the fragment is only executed once
      lazy val executedFragment = executeFragment(env)(fragment)
      val executeNow = env.arguments.sequential || fragment.execution.mustJoin

      val executingFragment = timedout(fragment, env.executionEnv.timer) {
        if (mustStop)             Task.now(fragment.skip)
        else if (executeNow)      Task.now(executedFragment)
        else                      start(executedFragment)(env)
      }(env.executionEnv.timeOut.getOrElse(fragment.execution.duration))

      // if this fragment is a join point, start a new sequence
      val nextBarrier =
        if (fragment.execution.mustJoin) Task.now(executedFragment.execution.result)
        // otherwise add the current execution to the sequence of current executions
        else                             barrier.map { case r => Result.ResultFailureMonoid.append(r, executedFragment.execution.result) }

      emit(executingFragment) fby sequencedExecution(env, nextBarrier, nextMustStop)
    }

  /** execute one fragment */
  def executeFragment(env: Env) = (fragment: Fragment) => {
    fragment.updateExecution { execution =>
      val timer = (new SimpleTimer).start
      execution.execute(env).setExecutionTime(timer.stop)
    }
  }

  def executeOnline(env: Env): Fragment => Process[Task, Fragment] = { fragment: Fragment =>
    fragment.execution.continuation match {
      case Some(continue) =>
        continue(fragment.executionResult).cata(
          fs => Process(fragment).toSource fby execute(env)(fs.contents),
          Process(fragment).toSource)

      case None => Process(fragment).toSource
    }
  }

  /** start the execution right away */
  def start[A](a: =>A)(env: Env) =
    new Task(Future(Task.Try(a))(env.executionEnv.executor).start)

  /** use the scalaz implementation of Timer to timeout the task */
  def timedout(fragment: Fragment, timer: Timer)(task: Task[Fragment])(duration: Duration): Task[Fragment] = {
    new Task(timer.withTimeout(task.get, duration.toMillis).map {
      case -\/(t)  => \/-(fragment.setExecution(fragment.execution.setResult(Skipped("timeout after "+duration))))
      case \/-(r)  => r
    })
  }
}

/**
 * helper functions for executing fragments
 */
object Executor extends Executor {
  def executeSpec(spec: SpecStructure, env: Env): SpecStructure =
    spec.|>((contents: Process[Task, Fragment]) => (contents |> sequencedExecution(env)).sequence(Runtime.getRuntime.availableProcessors))

  def runSpec(spec: SpecStructure, env: Env) =
    try executeSpec(spec, env).contents.runLog.run
    finally env.shutdown

  def runSpecification(spec: Specification) = {
    val env = Env(arguments = spec.structure(Env()).arguments)
    try runSpec(spec.structure(env), env)
    finally env.shutdown
  }

  /** only to be used in tests */
  def executeAll(seq: Fragment*)(implicit env: Env = Env()) = executeSeq(seq)(env)

  /** only to be used in tests */
  def executeSeq(seq: Seq[Fragment])(implicit env: Env = Env()) = {
    try { (Process(seq:_*).toSource |> executeTasks(env)).sequence(Runtime.getRuntime.availableProcessors).runLog.run }
    finally env.shutdown
  }

  /** synchronous execution */
  def executeFragments1 = {
    val env = Env()
    try process1.lift(executeFragment(env))
    finally env.shutdown
  }

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env) = process1.lift(executeFragment(env))
}
