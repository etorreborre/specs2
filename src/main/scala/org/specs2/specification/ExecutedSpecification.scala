package org.specs2
package specification
import ExecutedFragment._
import collection.Iterablex._
import internal.scalaz._
import concurrent._
import Scalaz._

/**
 * an executed specification with a name and a sequence of executed fragments
 */
case class ExecutedSpecification(name: SpecName, promised: Promise[Seq[ExecutedFragment]]) {
  def fragments = promised.get
  def includedLinkedSpecifications: Seq[ExecutedSpecStart] =
	  fragments collect { case s @ ExecutedSpecStart(_,_,_) if s.isIncludeLink => s }

  def includedSeeOnlySpecifications: Seq[ExecutedSpecStart] =
	  fragments collect { case s @ ExecutedSpecStart(_,_,_) if s.isSeeOnlyLink => s }
}

/**
 * for testing only
 */
private[specs2]
object ExecutedSpecification {
  def apply(fs: Seq[ExecutedFragment]): ExecutedSpecification = {
    fs match {
      case (s @ ExecutedSpecStart(_,_,_)) +: rest => ExecutedSpecification(s.specName, promise(fs))
      case other                                  => ExecutedSpecification(SpecName(""), promise(fs))
    }
  }
}
