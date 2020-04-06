package org.specs2
package specification
package process

import specification.core._

import scala.concurrent._
import duration._
import control._
import producer._, Producer._
import fp.syntax._
import main.Arguments

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
   *
   * The execution can be influenced by the specification itself if it specifies the `sequential` flag
   * for example
   */
  def execute(specArguments: Arguments): AsyncTransducer[Fragment, Fragment]
}

/**
 * Default execution for specifications:
 *
 *  - concurrent by default
 *  - using steps for synchronisation points
 */
case class DefaultExecutor(env: Env) extends Executor {

  /**
   * execute fragments:
   *
   *  - filter the ones that the user wants to keep
   *  - sequence the execution so that only parts in between steps are executed concurrently
   */
  def execute(specArguments: Arguments): AsyncTransducer[Fragment, Fragment] = { (contents: AsyncStream[Fragment]) =>
    sequencedExecution(specArguments)(contents).flatMap(executeOnline(specArguments))
  }

  /**
   * execute fragments, making sure that:
   *
   *  - "join" points are respected, i.e. when a Fragment is a join we must make sure that all previously
   *    executing fragments have finished their execution
   *
   *  - the fragments execute sequentially when args.sequential is true
   *
   *  - the execution stops if one fragment indicates that the result of the previous executions is not correct
   */
  def sequencedExecution(specArguments: Arguments): AsyncTransducer[Fragment, Fragment] = { (p: AsyncStream[Fragment]) =>
    type S = (Vector[Fragment], Vector[Fragment], Option[Fragment])
    val init: S = (Vector.empty, Vector.empty, None)
    val arguments = env.arguments.overrideWith(specArguments)

    val last: S => AsyncStream[Fragment] = {
      case (toStart, _, previousStep) =>
        emit(toStart.toList.map(_.startExecutionAfter(previousStep)(env)))
    }

    p.producerState(init, Option(last)) { case (fragment, (previous, previousStarted, previousStep)) =>
      if (arguments.skipAll)
        (one(if (fragment.isExecutable) fragment.skip else fragment), init)
      else if (arguments.sequential) {
        val started = fragment.startExecutionAfter(previousStarted.toList)(env)
        (one(started), (previous, previousStarted :+ started, None))
      }
      else {
        if (fragment.execution.mustJoin) {
          val started = previous.map(_.startExecutionAfter(previousStep)(env))
          val step =
            fragment.
              updateExecution(_.setErrorAsFatal).
              startExecutionAfter((previousStarted ++ started).toList)(env)

          (emit((started :+ step).toList), (Vector.empty, Vector.empty, Some(step)))
        }
        else if ((previous :+ fragment).count(_.isExecutable) >= arguments.batchSize) {
          val started = (previous :+ fragment).map(_.startExecutionAfter(previousStep)(env))
          (emit(started.toList), (Vector.empty, previousStarted ++ started, previousStep))
        }
        else
          (done[Action, Fragment], (previous :+ fragment, previousStarted, previousStep))
      }
    }

  }

  /** execute one fragment */
  def executeFragment(timeout: Option[FiniteDuration] = None)(fragment: Fragment): Fragment =
    fragment.updateExecution(executeExecution(timeout))

  /** execute one Execution */
  def executeExecution(timeout: Option[FiniteDuration] = None)(execution: Execution): Execution =
    timeout.fold(execution)(t => execution.setTimeout(t)).startExecution(env)

  def executeOnline(specArguments: Arguments)(fragment: Fragment): AsyncStream[Fragment] =
    fragment.execution.continuation match {
      case Some(continue) =>
        Producer.evalProducer(fragment.executionResult.map { result =>
          continue(result).fold(oneDelayed[Action, Fragment](fragment))(
            fs => oneDelayed[Action, Fragment](fragment) append execute(specArguments)(fs.contents))
        })

      case None => oneDelayed(fragment)
    }
}

/**
 * helper functions for executing fragments
 */
object DefaultExecutor {

  def executeSpec(spec: SpecStructure, env: Env): SpecStructure = {
    spec.|>((contents: AsyncStream[Fragment]) => contents |> DefaultExecutor(env).sequencedExecution(spec.arguments))
  }

  def runSpec(spec: SpecStructure, env: Env): List[Fragment] =
    executeSpec(spec, env).contents.runList.runMonoid(env.specs2ExecutionEnv)

  def runSpecification(spec: SpecificationStructure, env: Env): List[Fragment] = {
    lazy val structure = spec.structure(env)
    executeSpec(structure, env.copy(arguments = env.arguments <| structure.arguments)).contents.runList.
      runMonoid(env.specs2ExecutionEnv)
  }

  def runSpecificationFuture(spec: SpecificationStructure, env: Env): Future[List[Fragment]] = {
    lazy val structure = spec.structure(env)
    val env1 = env.copy(arguments = env.arguments <| structure.arguments)
    executeSpec(structure, env1).contents.runList.runFuture(env.specs2ExecutionEnv)
  }

  def runSpecificationAction(spec: SpecificationStructure, env: Env): Action[List[Fragment]] = {
    lazy val structure = spec.structure(env)
    val env1 = env.copy(arguments = env.arguments <| structure.arguments)
    executeSpec(structure, env1).contents.runList.
      flatMap { fs => fs.traverse(_.executionResult).as(fs) }
  }

  /** only to be used in tests */
  def executeFragments(fs: Fragments)(env: Env): List[Fragment] =
    fs.fragments.map(fs => executeAll(fs:_*)(env)).runMonoid(env.specs2ExecutionEnv)

  def executeAll(seq: Fragment*)(env: Env): List[Fragment] =
    executeSeq(seq)(env)

  def execute(f: Fragment)(env: Env): Fragment =
    executeAll(f)(env).headOption.getOrElse(f)

  /** only to be used in tests */
  def executeSeq(seq: Seq[Fragment])(env: Env): List[Fragment] =
    (emitSeq[Action, Fragment](seq) |> DefaultExecutor(env).sequencedExecution(Arguments())).runList.runMonoid(env.specs2ExecutionEnv)

  /** synchronous execution with a specific environment */
  def executeFragments1(env: Env): AsyncTransducer[Fragment, Fragment] = (p: AsyncStream[Fragment]) =>
    p.map(DefaultExecutor(env).executeFragment())
}
