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
  def execute(implicit arguments: Arguments): ExecutableSpecification => ExecutedSpecification
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

    val executed = spec.fs.foldLeft((Seq[ExecutedFragment](), true)) { (res, fs) =>
      val (executedFragments, executionOk) = res
      val fsArgs = arguments <| fs.arguments
      val executed = executeSequence(fs)(executionArgs(fsArgs, executionOk), Executor(executor))
      (executedFragments ++ executed, !fsArgs.stopOnFail || (executionOk && executed.forall(isOk)))
    }._1
    ExecutedSpecification(spec.name, executed, executor)
  }

  private def executionArgs(arguments: Arguments, previousExecutionOk: Boolean) =
    if (!arguments.stopOnFail || previousExecutionOk) arguments
    else                                              arguments <| args(skipAll=true)

  private def executeSequence(fs: FragmentSeq)(implicit args: Arguments, strategy: Strategy): Seq[ExecutedFragment] = {
    if (fs.fragments.size > 1 && !args.sequential) executeConcurrently(fs, args)(strategy)
    else                                           fs.fragments map executeFragment(args)
  }

  private def executeConcurrently(fs: FragmentSeq, args: Arguments)(implicit strategy: Strategy) = {
    fs.fragments.map {
      case f: Example  => PromisedExecutedFragment(promise(executeFragment(args)(f))(strategy))
      case f: Step     => PromisedExecutedFragment(promise(executeFragment(args)(f))(strategy))
      case f: Action   => PromisedExecutedFragment(promise(executeFragment(args)(f))(strategy))
      case f           => executeFragment(args)(f)
    }
  }
}
