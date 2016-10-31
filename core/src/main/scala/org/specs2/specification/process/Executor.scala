package org.specs2
package specification
package process

import execute._

import scalaz.{Failure => _, Success => _, _}
import Scalaz._
import specification.core._
import org.specs2.time.SimpleTimer

import scala.concurrent.duration.FiniteDuration
import control._
import eff._
import producer._
import Actions._
import Result.ResultFailureMonoid
import control.eff.syntax.async._

import scala.concurrent.TimeoutException

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
  def execute(env: Env): AsyncTransducer[Fragment, Fragment]
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
  def execute(env: Env): AsyncTransducer[Fragment, Fragment] = { contents: AsyncStream[Fragment] =>
    execute1(env)(contents).thenFinally(protect(env.shutdown))
  }

  /**
   * execute fragments possibly with a recursive call to execute1.
   *
   * The difference with `execute` is that `execute` shuts down the environment when the process is finished
   */
  def execute1(env: Env): AsyncTransducer[Fragment, Fragment] = { contents: AsyncStream[Fragment] =>
    sequencedExecution(env)(contents).sequence(env.arguments.threadsNb).flatMap(executeOnline(env))
  }

  /** a Process1 to execute fragments as tasks */
  def executeTasks(env: Env): AsyncTransducer[Fragment, Action[Fragment]] =
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
  def sequencedExecution(env: Env): AsyncTransducer[Fragment, Action[Fragment]] = {
    type Barrier = FatalExecution \/ Result
    type S = (Action[Barrier], Boolean)
    val init = (ok[Barrier](\/-(Success("barrier"))), false)

    transducers.stateEff[ActionStack, Fragment, Fragment, S](init) { case (fragment, (barrier, mustStop)) =>
      val arguments = env.arguments

      if (arguments.skipAll)
        (ok(if (fragment.isExecutable) fragment.skip else fragment), (barrier, mustStop))
      else {
        // if we need to wait, we do, and get the result
        val barrierResult =
          if (fragment.execution.mustJoin || arguments.sequential) attemptAction(barrier)(env.executionContext).fold(t => -\/(FatalExecution(t)), r => r)
          else \/-(Success("no barrier result"))

        // depending on the result we decide if we should go on executing fragments
        val barrierStop =
          mustStop ||
            (barrierResult match {
              case \/-(r) =>
                arguments.stopOnFail && r.isFailure ||
                arguments.stopOnSkip && r.isSkipped ||
                fragment.execution.nextMustStopIf(r)

              case -\/(f) => true
            })

        // if the previous fragments decided that we should stop the execution
        // skip the execution
        // otherwise execute synchronously or asynchronously
        val timeout = env.timeout.orElse(fragment.execution.timeout)

        if (mustStop)
          (ok(fragment.skip), (barrier, barrierStop))

        else if (fragment.execution.mustJoin) {
          val executedFragment: Throwable \/ Fragment =
            attemptAction(timedout(fragment, env)(asyncDelay(executeFragment(env.userEnv)(fragment)))(timeout))(env.executionContext)

          executedFragment match {
            case -\/(e) =>
              (ok(fragment.setExecution(Execution.fatal(e))), (ok(-\/(FatalExecution(e))), false))

            case \/-(ef) =>
              val (nextBarrier, stepStop) = {
                val stepResult = ef.executionFatalOrResult
                (stepResult, stepResult.fold(_ => true, r => fragment.execution.nextMustStopIf(r)))
              }
              (ok(ef), (ok(nextBarrier), barrierStop || stepStop))
          }
        } else if (env.arguments.sequential) {
          // stop right away if the previous fragment created a failed barrier
          if (barrierStop)
            (ok(fragment.skip), (barrier, barrierStop))
          else {
            lazy val executedFragment: Throwable \/ Fragment =
              attemptAction(timedout(fragment, env)(asyncDelay(executeFragment(env)(fragment)))(timeout))(env.executionContext)

            executedFragment match {
              case -\/(e) =>
                (ok(fragment.setExecution(Execution.fatal(e))), (ok(-\/(FatalExecution(e))), false))

              case \/-(ef) =>
                val nextBarrier = barrier.map {
                  case \/-(result) => ef.executionFatalOrResult.fold(f => -\/(f), r => \/-(result |+| r))
                  case f => f
                }
                (ok(ef), (nextBarrier, barrierStop))
            }
          }
        } else {
          val timeout = env.timeout.orElse(fragment.execution.timeout)

          val executingFragment: Action[Fragment] =
            timedout(fragment, env)(asyncFork(executeFragment(env.userEnv)(fragment)))(timeout)

          val newBarrier =
            Eff.EffApplicative[ActionStack].tuple2(barrier, executingFragment).map {
              case (\/-(result), ef) =>
                ef.executionFatalOrResult match {
                  case -\/(f) => -\/(f)
                  case \/-(r) => \/-(result |+| r)
                }

              case (-\/(e), _) => -\/(e)
            }
          (executingFragment, (newBarrier, barrierStop))
        }
      }
    }
  }

  /** execute one fragment */
  def executeFragment(env: Env) = (fragment: Fragment) => {
    fragment.updateExecution { execution =>
      val timer = (new SimpleTimer).start
      execution.execute(env).setExecutionTime(timer.stop)
    }
  }

  def executeOnline(env: Env): Fragment => AsyncStream[Fragment] = { fragment: Fragment =>
    fragment.execution.continuation match {
      case Some(continue) =>
        continue(fragment.executionResult).cata(
          fs => emitAsyncDelayed(fragment) append execute1(env)(fs.contents),
          emitAsyncDelayed(fragment))

      case None => emitAsyncDelayed(fragment)
    }
  }

  /** use the scalaz implementation of Timer to timeout the task */
  def timedout(fragment: Fragment, env: Env)(task: Action[Fragment])(duration: Option[FiniteDuration]): Action[Fragment] = {
    duration match {
      case None    => task
      case Some(d) =>
        withTimeout(task)(d, env.executionEnv).attempt.map {
          case -\/(t: TimeoutException)  => fragment.setExecution(fragment.execution.setResult(Skipped("timeout after "+d)))
          case -\/(t)  => fragment.setExecution(fragment.execution.setResult(Error(t)))
          case \/-(r)  => r
        }
    }
  }
}

/**
 * helper functions for executing fragments
 */
object DefaultExecutor extends DefaultExecutor {

  def executeSpecWithoutShutdown(spec: SpecStructure, env: Env): SpecStructure =
    spec.|>((contents: AsyncStream[Fragment]) => (contents |> sequencedExecution(env)).sequence(env.arguments.threadsNb))

  def executeSpec(spec: SpecStructure, env: Env): SpecStructure = {
    spec.|>((contents: AsyncStream[Fragment]) => (contents |> sequencedExecution(env)).
      sequence(env.arguments.threadsNb).thenFinally(protect(env.shutdown)))
  }

  def runSpec(spec: SpecStructure, env: Env): List[Fragment] =
    runAction(executeSpec(spec, env).contents.runList)(env.executionContext).toOption.getOrElse(Nil)

  def runSpecification(spec: SpecificationStructure) = {
    lazy val structure = spec.structure(Env())
    val env = Env(arguments = structure.arguments)
    runSpec(structure, env)
  }

  /** only to be used in tests */
  def executeFragments(fs: Fragments)(implicit env: Env = Env()) = executeAll(fs.fragments:_*)
  def executeAll(seq: Fragment*)(implicit env: Env = Env()) = executeSeq(seq)(env)
  def execute(f: Fragment)(implicit env: Env = Env()) = executeAll(f)(env).headOption.getOrElse(f)

  /** only to be used in tests */
  def executeSeq(seq: Seq[Fragment])(implicit env: Env = Env()): List[Fragment] =
    try runAction((emitAsync(seq:_*) |> executeTasks(env)).sequence(env.arguments.threadsNb).runList)(env.executionContext).toOption.getOrElse(Nil)
    finally env.shutdown

  /** synchronous execution */
  def executeFragments1 =
    transducers.transducer[ActionStack, Fragment, Fragment](executeFragment(Env()))

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env) = transducers.transducer(executeFragment(env))
}
