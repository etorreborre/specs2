package org.specs2
package reporter

import java.util.concurrent.Executors
import execute.{ Error, Failure }
import org.specs2.internal.scalaz._
import Scalaz._
import concurrent._
import Strategy.Executor
import specification._
import control.NamedThreadFactory
import main.{ArgumentsArgs, Arguments}
import scala.collection.immutable.List._
import main.Arguments._

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
    val executed = spec.fs.foldLeft((Nil:List[ExecutedFragment], true)) { (res, fs) =>
      val (executedFragments, executionOk) = res
      val fsArgs = arguments <| fs.arguments
      val executed = executeSequence(fs)(executionArgs(fsArgs, executionOk))
      (executedFragments ++ executed, !fsArgs.stopOnFail || (executionOk && executed.forall(isOk(_))))
    }._1
    ExecutedSpecification(spec.name, executed)
  }

  private def executionArgs(arguments: Arguments, previousExecutionOk: Boolean) =
    if (!arguments.stopOnFail || previousExecutionOk) arguments
    else (arguments <| args(skipAll=true))

  /**
   * @return true if the executed fragment is not a Failure or an Error
   */
  private def isOk(e: ExecutedFragment) = e match {
    case ExecutedResult(_,r,_,_,_) if r.isFailure || r.isError => false
    case other                                                => true
  }

  private def executeSequence(fs: FragmentSeq)(implicit args: Arguments): Seq[ExecutedFragment] = {
    if (fs.fragments.size > 1 && !args.sequential)
      executeConcurrently(fs, args)
    else
      fs.fragments.map(f => executeFragment(args)(f))
  }

  private def executeConcurrently(fs: FragmentSeq, args: Arguments) = {
    implicit val executor = Executors.newFixedThreadPool(args.threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
    try {
      fs.fragments.map(f => promise(executeFragment(args)(f))).sequence.get
    } finally {
      executor.shutdown()
    }
  }
}
