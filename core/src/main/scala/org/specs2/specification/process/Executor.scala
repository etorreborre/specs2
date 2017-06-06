package org.specs2
package specification
package process

import java.util.concurrent.TimeoutException

import scalaz.{Failure => _, Success => _, _}
import Scalaz._
import specification.core._

import scala.concurrent._
import duration._
import control._
import producer._
import producers._
import Actions._
import org.specs2.control.eff.syntax.all._
import org.specs2.execute.{Result, Skipped, Error}

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
    sequencedExecution(env)(contents).flatMap(executeOnline(env))
  }

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
  def sequencedExecution(env: Env): AsyncTransducer[Fragment, Fragment] = {
    type S = (Vector[Fragment], Vector[Fragment], Boolean)
    val init: S = (Vector.empty, Vector.empty, false)
    val arguments = env.arguments

    def executeFragments(fs: Seq[Fragment], timeout: Option[FiniteDuration]): AsyncStream[Fragment] =
      if (arguments.sequential) emitEff(fs.toList.traverse(f => executeOneFragment(f, timeout)))
      else                      emitEff(fs.toList.traverseA(f => executeOneFragment(f, timeout)))

    def executeOneFragment(f: Fragment, timeout: Option[FiniteDuration]): Action[Fragment] = {
      if (arguments.sequential) asyncDelayAction(executeFragment(env, timeout)(f))
      else                      asyncForkAction(executeFragment(env, timeout)(f), env.executionContext).futureAttempt.map {
        case Left(t: TimeoutException) => executeFragment(env, timeout)(f.setExecution(Execution.result(Skipped("timeout"+timeout.map(" after "+_).getOrElse("")))))
        case Left(t)                   => executeFragment(env, timeout)(f.setExecution(Execution.result(Error(t))))
        case Right(f1)                 => f1
      }
    }

    val last: S => AsyncStream[Fragment] = {
      case (toStart, _, mustStop) =>
        if (mustStop) emit(toStart.toList.map(_.skip))
        else          executeFragments(toStart, env.timeout)
    }

    transducers.producerStateEff(init, Option(last)) { case (fragment, (fragments, started, mustStop)) =>
      val timeout = env.timeout.orElse(fragment.execution.timeout)

      def stopAll(previousResults: Result, fragment: Fragment): Boolean = {
        mustStop ||
        arguments.stopOnFail && previousResults.isFailure ||
        arguments.stopOnSkip && previousResults.isSkipped ||
        fragment.execution.nextMustStopIf(previousResults) ||
        fragment.executionFatalOrResult.isLeft
      }

      if (arguments.skipAll || mustStop)
        ok((one(if (fragment.isExecutable) fragment.skip else fragment), (Vector.empty, started, mustStop)))
      else if (arguments.sequential)
        executeOneFragment(fragment, timeout).flatMap { f =>
          ok((one(f), (Vector.empty, started, stopAll(f.executionResult, f))))
        }
      else {
        if (fragment.execution.mustJoin) {
          executeFragments(fragments, timeout).run.flatMap {
            case Done() =>
              executeOneFragment(fragment, timeout).flatMap { step =>
                step.finishExecution
                ok((one(step), (Vector.empty, Vector.empty, stopAll(started.foldMap(_.executionResult), step))))
              }

            case producer.One(f) =>
              // wait for f to finish executing
              f.finishExecution
              executeOneFragment(fragment, timeout).flatMap { step =>
                ok((oneOrMore(f, List(step)), (Vector.empty, Vector.empty, stopAll((started :+ f).foldMap(_.executionResult), step))))
              }

            case fs @ More(as, next) =>
              // wait for as to finish executing
              as.map(_.finishExecution)
              executeOneFragment(fragment, timeout).flatMap { step =>
                ok((emitAsync(as:_*) append next append one(step),
                  (Vector.empty, Vector.empty, stopAll((started ++ as).foldMap(_.executionResult), step))))
              }
          }
        }
        else if (fragments.count(_.isExecutable) >= arguments.batchSize)
          executeFragments(fragments :+ fragment, timeout).run.flatMap {
            case Done()          => ok((done, (Vector.empty, started, mustStop)))
            case producer.One(f) => ok((one(f), (Vector.empty, started, mustStop)))
            case More(as, next)  => ok((emitAsync(as:_*) append next, (Vector.empty, as.toVector, mustStop)))
          }
        else
          ok((done[ActionStack, Fragment], (fragments :+ fragment, started, mustStop)))
      }
    }

  }

  /** execute one fragment */
  def executeFragment(env: Env, timeout: Option[FiniteDuration] = None) = (fragment: Fragment) => {
    timeout.fold(fragment)(t => fragment.setTimeout(t)).updateExecution { execution =>
      execution.startExecution(env)
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
}

/**
 * helper functions for executing fragments
 */
object DefaultExecutor extends DefaultExecutor {

  def executeSpecWithoutShutdown(spec: SpecStructure, env: Env): SpecStructure =
    spec.|>((contents: AsyncStream[Fragment]) => contents |> sequencedExecution(env))

  def executeSpec(spec: SpecStructure, env: Env): SpecStructure = {
    spec.|>((contents: AsyncStream[Fragment]) => (contents |> sequencedExecution(env)).thenFinally(protect(env.shutdown)))
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
    try runAction((emitAsync(seq:_*) |> sequencedExecution(env)).runList).toOption.getOrElse(Nil)
    finally env.shutdown

  /** synchronous execution */
  def executeFragments1: AsyncTransducer[Fragment, Fragment] =
    executeFragments1(Env())

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env): AsyncTransducer[Fragment, Fragment] =
    transducers.transducer[ActionStack, Fragment, Fragment](executeFragment(env))
}
