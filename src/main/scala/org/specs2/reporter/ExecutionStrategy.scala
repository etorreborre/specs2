package org.specs2
package reporter

import java.util.concurrent.Executors
import scalaz._ 
import Scalaz._ 
import concurrent._
import Strategy.Executor
import main.Arguments
import specification._

/**
 * Generic trait for executing Fragments, which are sorted according to their dependencies
 */
private[specs2]
trait ExecutionStrategy {
  def execute(implicit arguments: Arguments): Seq[Seq[Fragment]] => Seq[ExecutedFragment] 
}

/**
 * This trait uses Scalaz promises to execute Fragments concurrently
 * 
 * It uses a Fixed thread pool with a number of threads to execute the fragments.
 * The default number is 4.
 */
private[specs2]
trait DefaultExecutionStrategy extends ExecutionStrategy with FragmentExecution {
  
  def execute(implicit arguments: Arguments) = (fragments: Seq[Seq[Fragment]]) => {
    implicit val executor = Executors.newFixedThreadPool(arguments.threadsNb)
    try {
      fragments.map(l => l.map(f => promise(executeFragment(f))).sequence.get).flatten
    } finally {
      executor.shutdown()
    }
  }
}
