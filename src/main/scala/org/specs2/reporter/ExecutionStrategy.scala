package org.specs2
package reporter

import java.util.concurrent.Executors
import execute.{ Error, Failure }
import org.specs2.internal.scalaz._
import Scalaz._
import concurrent._
import Strategy._
import specification._
import control.NamedThreadFactory
import main.{ArgumentsArgs, Arguments}
import scala.collection.immutable.List._
import main.Arguments._
import ExecutedFragment._

/**
 * Generic trait for executing Fragments, which are sorted according to their dependencies
 */
private[specs2]
trait ExecutionStrategy {
  def execute(implicit arguments: Arguments): ExecutableSpecification => ExecutingSpecification
}

/**
 * This trait uses Scalaz promises to execute Fragments concurrently
 * 
 * It uses a Fixed thread pool with a number of threads to execute the fragments.
 * The default number is the number of available threads by default but this can be overriden by providing different Arguments
 */
private[specs2]
trait DefaultExecutionStrategy extends ExecutionStrategy with FragmentExecution {
  import ArgumentsArgs._

  /**
   * execute sequences of Fragments.
   *
   * If the stopOnFail argument is true, we check that the execution is ok before executing the next sequence.
   */
  def execute(implicit arguments: Arguments) = (spec: ExecutableSpecification) => {
    implicit val executor = Executors.newFixedThreadPool(spec.arguments.threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
    try {
      val executing = spec.fs.foldLeft((Seq[ExecutingFragment](), () => 1:Any, true)) { (res, fs) =>
        val (executingFragments, barrier, executionOk) = res
        val fsArgs = arguments <| fs.arguments
        val executing = executeSequence(fs, barrier())(executionArgs(fsArgs, executionOk), Executor(executor))

        (executingFragments ++ executing,
         () => executing.map(_.get),
         !fsArgs.stopOnFail || (executionOk && executing.forall(f => isOk(f.get))))
      }._1
      ExecutingSpecification(spec.name, executing, executor)
    } catch {
      // just in case something bad happens, or if there's an InterruptedException, shutdown the executor
      case e => executor.shutdown; throw e
    }
  }

  private def executionArgs(arguments: Arguments, previousExecutionOk: Boolean) =
    if (!arguments.stopOnFail || previousExecutionOk) arguments
    else                                              arguments <| args(skipAll=true)

  private def executeSequence(fs: FragmentSeq, barrier: =>Any)(implicit args: Arguments, strategy: Strategy): Seq[ExecutingFragment] = {
    if (!args.sequential) executeConcurrently(fs, barrier, args)(strategy)
    else                  fs.fragments.map(f => FinishedExecutingFragment(executeFragment(args)(f)))
  }

  private def executeConcurrently(fs: FragmentSeq, barrier: =>Any, args: Arguments)(implicit strategy: Strategy) = {
    def executeWithBarrier(f: Fragment) = { barrier; executeFragment(args)(f) }
    fs.fragments.map {
      case f: Example  => PromisedExecutingFragment(promise(executeWithBarrier(f))(strategy))
      case f: Action   => PromisedExecutingFragment(promise(executeWithBarrier(f))(strategy))
      case f: Step     => LazyExecutingFragment(() => executeWithBarrier(f))
      case f           => FinishedExecutingFragment(executeWithBarrier(f))
    }
  }
}
