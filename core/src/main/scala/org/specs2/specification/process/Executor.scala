package org.specs2
package specification
package process

import data.Processes

import scalaz.stream._
import scalaz.stream.Process.{Env =>_,_}
import execute._
import scalaz.{Success=>_, Failure=>_,_}, Scalaz._
import specification.core._
import org.specs2.time.SimpleTimer
import scalaz.concurrent.Task
import scala.concurrent.duration.FiniteDuration
import Processes._

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
  def execute(env: Env): Process[Task, Fragment] => Process[Task, Fragment]
}

/**
 * Default execution for specifications:
 * 
 *  - concurrent by default
 *  - using steps for synchronisation points
 */
trait DefaultExecutor extends Executor {

  /**
   * execute fragments:
   *
   *  - filter the ones that the user wants to keep
   *  - sequence the execution so that only parts in between steps are executed concurrently
   */
  def execute(env: Env): Process[Task, Fragment] => Process[Task, Fragment] = { contents: Process[Task, Fragment] =>
     execute1(env)(contents).andFinally(Task.delay(env.shutdown))
  }

  /**
   * execute fragments possibly with a recursive call to execute1.
   *
   * The difference with `execute` is that `execute` shuts down the environment when the process is finished
   */
  def execute1(env: Env): Process[Task, Fragment] => Process[Task, Fragment] = { contents: Process[Task, Fragment] =>
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
  def sequencedExecution(env: Env, barrier: Task[Result] = Task.now(Success("barrier")), mustStop: Boolean = false): Process1[Fragment, Task[Fragment]] =
    receive1 { fragment: Fragment =>
      val arguments = env.arguments

      if (arguments.skipAll) {
        val skipped =
          if (fragment.isExecutable) emit(Task.now(fragment.skip))
          else                       emit(Task.now(fragment))

        skipped fby sequencedExecution(env, barrier, mustStop)
      } else {
        // if we need to wait, we do, and get the result
        val barrierResult =
          if (fragment.execution.mustJoin) barrier.attemptRun.fold(t => org.specs2.execute.Error(t), r => r)
          else                             Success("no barrier result")

        // depending on the result we decide if we should go on executing fragments
        val barrierStop =
          mustStop ||
            arguments.stopOnFail && barrierResult.isFailure ||
            arguments.stopOnSkip && barrierResult.isSkipped ||
            fragment.execution.nextMustStopIf(barrierResult)

        // if the previous fragments decided that we should stop the execution
        // skip the execution
        // otherwise execute synchronously or asynchronously

        // make sure the fragment is only executed once
        lazy val executedFragment = executeFragment(env)(fragment)
        val executeNow = env.arguments.sequential || fragment.execution.mustJoin

        val executingFragment = timedout(fragment, env) {
          if (mustStop)             Task.now(fragment.skip)
          else if (executeNow)      Task.delay(executedFragment)
          else                      start(executedFragment)(env.executorService)
        }(env.timeout.orElse(fragment.execution.timeout))

        // if this fragment is a join point, start a new sequence
        // and check if the execution needs to be stopped in case of a step error
        val (nextBarrier, stepStop) =
          if (fragment.execution.mustJoin) {
            val stepResult = executedFragment.executionResult
            (Task.now(stepResult), fragment.execution.nextMustStopIf(stepResult))
          }
          // otherwise add the current execution to the sequence of current executions
          else
            (barrier.map { case r => Result.ResultFailureMonoid.append(r, executedFragment.execution.result) }, false)

        emit(executingFragment) fby sequencedExecution(env, nextBarrier, barrierStop || stepStop)
      }
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
          fs => Process(fragment).toSource fby execute1(env)(fs.contents),
          Process(fragment).toSource)

      case None => Process(fragment).toSource
    }
  }

  /** use the scalaz implementation of Timer to timeout the task */
  def timedout(fragment: Fragment, env: Env)(task: Task[Fragment])(duration: Option[FiniteDuration]): Task[Fragment] = {
    duration match {
      case None    => task
      case Some(d) =>
        new Task(env.executionEnv.withTimeout(task.get, d.toMillis).map {
          case -\/(t)  => \/-(fragment.setExecution(fragment.execution.setResult(Skipped("timeout after "+d))))
          case \/-(r)  => r
        })
    }
  }
}

/**
 * helper functions for executing fragments
 */
object DefaultExecutor extends DefaultExecutor {

  def executeSpecWithoutShutdown(spec: SpecStructure, env: Env): SpecStructure =
    spec.|>((contents: Process[Task, Fragment]) => (contents |> sequencedExecution(env)).sequence(Runtime.getRuntime.availableProcessors))

  def executeSpec(spec: SpecStructure, env: Env): SpecStructure =
    spec.|>((contents: Process[Task, Fragment]) => (contents |> sequencedExecution(env)).sequence(Runtime.getRuntime.availableProcessors).andFinally(Task.delay(env.shutdown)))

  def runSpec(spec: SpecStructure, env: Env): IndexedSeq[Fragment] =
    executeSpec(spec, env).contents.runLog.run

  def runSpecification(spec: SpecificationStructure) = {
    lazy val structure = spec.structure(Env())
    val env = Env(arguments = structure.arguments)
    runSpec(structure, env)
  }

  /** only to be used in tests */
  def executeFragments(fs: Fragments)(implicit env: Env = Env()) = executeAll(fs.fragments:_*)
  def executeAll(seq: Fragment*)(implicit env: Env = Env()) = executeSeq(seq)(env)
  def execute(f: Fragment)(implicit env: Env = Env()) = executeAll(f)(env).head

  /** only to be used in tests */
  def executeSeq(seq: Seq[Fragment])(implicit env: Env = Env()): IndexedSeq[Fragment] =
    try { (Process(seq:_*).toSource |> executeTasks(env)).sequence(Runtime.getRuntime.availableProcessors).runLog.run }
    finally env.shutdown

  /** synchronous execution */
  def executeFragments1 =
    process1.lift(executeFragment(Env()))

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env) = process1.lift(executeFragment(env))
}
