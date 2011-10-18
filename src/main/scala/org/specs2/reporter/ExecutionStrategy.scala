package org.specs2
package reporter

import java.util.concurrent.Executors
import org.specs2.internal.scalaz._
import Scalaz._
import concurrent._
import Strategy.Executor
import specification._
import control.NamedThreadFactory
import main.{ArgumentsArgs, Arguments}
import specification.FragmentExecution._

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
//    val executed = spec.fs.foldLeft((promise(Seq[ExecutedFragment]()), true)) { (res, fs) =>
//      val (executedFragments, executionOk) = res
//      val fsArgs = arguments <| fs.arguments
//      val executed = executeSequence(fs)(executionArgs(fsArgs, executionOk))
//      ((executedFragments <**> executed)(_ ++ _), !fsArgs.stopOnFail || (executionOk && executed.forall(isOk(_))))
//    }._1
    val executed = spec.fs.traverse(execution).fragments
    ExecutedSpecification(spec.name, executed)
  }

  private def executionArgs(arguments: Arguments, previousExecutionOk: Boolean) =
    if (!arguments.stopOnFail || previousExecutionOk) arguments
    else (arguments <| args(skipAll=true))
}

object DefaultExecutionStrategy {

  def executionState(arguments: Arguments): State[(Boolean, Seq[ExecutedFragment]), Boolean] = state { (executionOk, fs) =>
    val continueExecution = !arguments.stopOnFail || executionOk && fs.forall(isOk)
    ((continueExecution, fs), continueExecution)
  }

  def fragmentsExecution = (fs: Seq[ExecutedFragment]) => {
    executeSequence(fs)(fs.args)
  }

  def execution: Function1[Seq[ExecutedFragment], PromisedExecutedFragments]= (fs: Seq[ExecutedFragment]) => {
    new PromisedExecutedFragments()
  }

  def executeSequence(fs: FragmentSeq)(implicit args: Arguments): Promise[Seq[ExecutedFragment]] = {
    if (fs.fragments.size > 1 && !args.sequential)
      executeConcurrently(fs, args)
    else
      promise(fs.fragments.map(f => executeFragment(args)(f)))
  }

  def executeConcurrently(fs: FragmentSeq, args: Arguments) = {
    implicit val executor = Executors.newFixedThreadPool(args.threadsNb, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
    try {
      fs.fragments.map(f => promise(executeFragment(args)(f))).sequence
    } finally {
      executor.shutdown()
    }
  }
  /**
   * @return true if the executed fragment is not a Failure or an Error
   */
  def isOk(e: ExecutedFragment) = e match {
    case ExecutedResult(_,r,_,_,_) if r.isFailure || r.isError => false
    case other                                                 => true
  }

  class PromisedExecutedFragments {
    def fragments:
  }
}