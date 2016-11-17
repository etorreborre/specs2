package org.specs2
package specification
package process

import execute._

import scalaz.{Failure => _, Success => _, _}
import Scalaz._
import specification.core._
import org.specs2.time.SimpleTimer

import scala.concurrent._, duration._
import control._
import producer._
import Actions._
import Result.ResultFailureMonoid
import org.specs2.control.eff.{Async, AsyncFutureInterpreter}

import scala.concurrent.{Await, Future, Promise}

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
    execute1(env)(contents).andFinally(protect(env.shutdown))
  }

  /**
   * execute fragments possibly with a recursive call to execute1.
   *
   * The difference with `execute` is that `execute` shuts down the environment when the process is finished
   */
  def execute1(env: Env): AsyncTransducer[Fragment, Fragment] = { contents: AsyncStream[Fragment] =>
    sequencedExecution(env)(contents).sequence[Async](env.arguments.threadsNb).flatMap(executeOnline(env))
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
    type Barrier = (List[Future[Any]], FatalExecution \/ Result)
    type S = (Action[Barrier], Boolean)
    val async = AsyncFutureInterpreter.create(env.executionContext)

    val init = (ok[Barrier]((Nil, \/-(Success("barrier")))), false)

    transducers.stateEff[ActionStack, Fragment, Fragment, S](init) { case (fragment, (barrier, mustStop)) =>
      val arguments = env.arguments
      val timeout = env.timeout.orElse(fragment.execution.timeout)

      if (arguments.skipAll)
        (ok(if (fragment.isExecutable) fragment.skip else fragment), (barrier, mustStop))
      else {
        // if we need to wait, we do, and get the result
        val barrierResult =
          if (fragment.execution.mustJoin || arguments.sequential) {
            attemptAction {
              barrier.map {
                case (futures, \/-(r)) =>
                  implicit val ec = env.executionContext
                  Await.result(Future.sequence(futures), scala.concurrent.duration.Duration.Inf)
                  \/-(r)
//                  .map(_.executionFatalOrResult match {
//                  case \/-(r) => \/-(result |+| r)
//                  case -\/(f) => -\/(f)
//                  case other  => \/-(result)
//                }
                case (_, other) => other
              }
            }.fold(t => -\/(FatalExecution(t)), r => r)
          }
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

        if (mustStop)
          (ok(fragment.skip), (barrier, barrierStop))

        else if (fragment.execution.mustJoin) {
          val executedFragment: Throwable \/ Fragment =
            attemptAction(timedout(fragment, env)(asyncDelayAction(executeFragment(env.userEnv)(fragment)))(timeout))

          executedFragment match {
            case -\/(e) =>
              (ok(fragment.setExecution(Execution.fatal(e))), (ok[Barrier]((Nil, -\/(FatalExecution(e)))), false))

            case \/-(ef) =>
              val (nextBarrier, stepStop) = {
                val stepResult = ef.executionFatalOrResult
                (stepResult, stepResult.fold(_ => true, r => fragment.execution.nextMustStopIf(r)))
              }
              (ok(ef), (ok((Nil, nextBarrier)), barrierStop || stepStop))
          }
        } else if (env.arguments.sequential) {
          // stop right away if the previous fragment created a failed barrier
          if (barrierStop)
            (ok(fragment.skip), (barrier, barrierStop))
          else {
            lazy val executedFragment: Throwable \/ Fragment =
              attemptAction(timedout(fragment, env)(asyncDelayAction(executeFragment(env)(fragment)))(timeout))

            executedFragment match {
              case -\/(e) =>
                (ok(fragment.setExecution(Execution.fatal(e))), (ok((Nil, -\/(FatalExecution(e)))), false))

              case \/-(ef) =>
                val nextBarrier = barrier.map {
                  case (fs, \/-(result)) => (fs, ef.executionFatalOrResult.fold(f => -\/(f), r => \/-(result |+| r)))
                  case f => f
                }
                (ok(ef), (nextBarrier, barrierStop))
            }
          }
        } else {

          val execution: Promise[Fragment] = Promise()
          lazy val executed = executeFragment(env.userEnv)(fragment)
          val fut = execution.complete(scala.util.Success(executed)).future

          val executingFragment: Action[Fragment] =
            timedout(fragment, env)(asyncForkAction {
              executed
            })(timeout)

          val newBarrier =
            barrier.map { case (futures, result) => (fut :: futures, result) }

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
      case None => task
      case Some(d) => task
//        withTimeout(task)(d, env.executionEnv).asyncAttempt.map {
//          case Left(t: TimeoutException)  => fragment.setExecution(fragment.execution.setResult(Skipped("timeout after "+d)))
//          case Left(t)  => fragment.setExecution(fragment.execution.setResult(Error(t)))
//          case Right(r)  => r
//        }
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
    runAction(executeSpec(spec, env).contents.runList).toOption.getOrElse(Nil)

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
    try runAction((emitAsync(seq:_*) |> executeTasks(env)).sequence(env.arguments.threadsNb).runList).toOption.getOrElse(Nil)
    finally env.shutdown

  /** synchronous execution */
  def executeFragments1 =
    transducers.transducer[ActionStack, Fragment, Fragment](executeFragment(Env()))

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env) = transducers.transducer(executeFragment(env))
}
