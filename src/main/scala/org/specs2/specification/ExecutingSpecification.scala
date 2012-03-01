package org.specs2
package specification

import collection.Iterablex._
import java.util.concurrent.{Executors, ExecutorService}
import control.NamedThreadFactory
import ExecutingSpecification._

/**
 * a specification with a name and a sequence of fragments being executed
 */
case class ExecutingSpecification(name: SpecName, fs: Seq[ExecutingFragment], executor: ExecutorService = newExecutor) {

  /** @return the executed fragments, but as a lazy list */
  lazy val execute = ExecutedSpecification(name, foreach { (n, fs) => fs.view.map(_.get) })

  /** @return the executed fragments */
  def executed = execute

  /** @return a lazy list where each fragment will be executed on access */
  def foreach[T](f: (SpecName, Seq[ExecutedFragment]) => T) =
    try { f(name, fs.view.map(_.get)) } finally { terminate }

  /** @return an ExecutingSpecification where each executed fragment is mapped to another one */
  def map(f: ExecutedFragment => ExecutedFragment) =  copy(fs = fs.map(_.map(f)))

  override def toString = fs.mkString("\n")

  def terminate = executor.shutdown()
}

/**
 * for testing only
 */
private[specs2]
object ExecutingSpecification {

  /**
   * @return an ExecutedSpecification from a sequence of already executed fragments
   */
  def create(fs: Seq[ExecutedFragment]): ExecutingSpecification = apply(fs.map(f => FinishedExecutingFragment(f)))
  /**
   * @return an ExecutedSpecification from a sequence of already executed fragments
   */
  def create(name: SpecName, fs: Seq[ExecutedFragment]): ExecutingSpecification =
    ExecutingSpecification(name, fs.map(f => FinishedExecutingFragment(f)))

  /**
   * @return an ExecutedSpecification from a sequence of fragments being executed
   */
  def apply(fs: Seq[ExecutingFragment]): ExecutingSpecification = {
    fs match {
      case (FinishedExecutingFragment(s @ ExecutedSpecStart(_,_,_))) +: rest => ExecutingSpecification(s.specName, fs)
      case other                                                             => ExecutingSpecification(SpecName(""), fs)
    }
  }

  private def newExecutor = {
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
  }

}
