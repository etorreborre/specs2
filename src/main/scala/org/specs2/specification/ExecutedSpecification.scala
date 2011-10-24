package org.specs2
package specification
import ExecutedFragment._
import collection.Iterablex._
import java.util.concurrent.{Executors, ExecutorService}
import control.NamedThreadFactory
import ExecutedSpecification._
/**
* an executed specification with a name and a sequence of executed fragments
*/
case class ExecutedSpecification(name: SpecName, fs: Seq[ExecutedFragment], executor: ExecutorService = newExecutor) {
  def includedLinkedSpecifications: Seq[ExecutedSpecStart]  = fragments collect isIncludeLink
  def includedSeeOnlySpecifications: Seq[ExecutedSpecStart] = fragments collect isSeeOnlyLink

  def fragments = foreach { (n, fs) => fs.map { f => f match { case t: PromisedExecutedFragment => t.get; case other => other }} }

  def foreach[T](f: (SpecName, Seq[ExecutedFragment]) => T) =
    try { f(name, fs) } finally { terminate }

  def terminate = executor.shutdown()
}

/**
 * for testing only
 */
private[specs2]
object ExecutedSpecification {

  def apply(fs: Seq[ExecutedFragment]): ExecutedSpecification = {
    fs match {
      case (s @ ExecutedSpecStart(_,_,_)) +: rest => ExecutedSpecification(s.specName, fs)
      case other                                  => ExecutedSpecification(SpecName(""), fs)
    }
  }

  private def newExecutor = {
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, new NamedThreadFactory("specs2.DefaultExecutionStrategy"))
  }

}
