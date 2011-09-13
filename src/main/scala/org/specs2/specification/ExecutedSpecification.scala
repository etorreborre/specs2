package org.specs2
package specification
import ExecutedFragment._
import collection.Iterablex._

/**
 * an executed specification with a name and a sequence of executed fragments
 */
case class ExecutedSpecification(name: SpecName, fragments: Seq[ExecutedFragment]) {
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
      case (s @ ExecutedSpecStart(_,_,_)) +: rest => ExecutedSpecification(s.specName, fs)
      case other                                  => ExecutedSpecification(SpecName(""), fs)
    }
  }
}
