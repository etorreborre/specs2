package org.specs2
package specification

import main.Arguments
import text._
import time.SimpleTimer
import execute._
import main._
import io.Location
/**
 * The Executed Fragments represent pieces of a Specification
 * which have already been executed to provide a Result
 */
sealed trait ExecutedFragment {
  /** @return the location of the executed Fragment */
  def location: Location

  def stats: Stats
}

private[specs2]
object ExecutedFragment {
  /** @return true if the ExecutedFragment is a Text */
  def isExecutedText: Function[ExecutedFragment, Boolean] = { case ExecutedText(_, _) => true; case _ => false }
  /** @return the text if the Fragment is a Text */
  def isSomeExecutedText: PartialFunction[ExecutedFragment, ExecutedText] = { case t @ ExecutedText(_, _) => t }
  /** @return true if the ExecutedFragment is a result */
  def isExecutedResult: Function[ExecutedFragment, Boolean] = { case ExecutedResult(_,_,_,_,_) => true; case _ => false }
  /** @return true if the ExecutedFragment is a Start */
  def isExecutedSpecStart: Function[ExecutedFragment, Boolean] = { case ExecutedSpecStart(_,_,_) => true; case _ => false }
  /** @return true if the ExecutedFragment is an End */
  def isExecutedSpecEnd: Function[ExecutedFragment, Boolean] = { case ExecutedSpecEnd(_,_,_) => true; case _ => false }
}

case class ExecutedText(text: String, location: Location = new Location) extends ExecutedFragment {
  def stats: Stats = Stats()
}
case class ExecutedResult(s: MarkupString, result: Result, timer: SimpleTimer, location: Location, statistics: Stats) extends ExecutedFragment { outer =>
  def text(implicit args: Arguments) = s match {
    case CodeMarkup(s) if (!result.expected.isEmpty && !args.fromSource) => CodeMarkup(result.expected)
    case _                                                               => s
  }
  def stats = statistics.copy(timer = outer.timer)
  def isSuccess = stats.isSuccess
}
private[specs2]
object ExecutedResult {
  def apply(desc: String, r: Result): ExecutedResult = ExecutedResult(NoMarkup(desc), r, new SimpleTimer, new Location, Stats())
}

trait ExecutedStandardFragment extends ExecutedFragment {
  val stats: Stats = Stats()
}
case class ExecutedBr(location: Location = new Location) extends ExecutedStandardFragment
case class ExecutedEnd(location: Location = new Location) extends ExecutedStandardFragment
case class ExecutedTab(n: Int = 1, location: Location = new Location) extends ExecutedStandardFragment
case class ExecutedBacktab(n: Int = 1, location: Location = new Location) extends ExecutedStandardFragment

case class ExecutedSpecStart(start: SpecStart, location: Location = new Location, stats: Stats = Stats()) extends ExecutedFragment {
  
  def isSeeOnlyLink = start.isSeeOnlyLink
  def isIncludeLink = start.isIncludeLink
  def isLink        = start.isLink
  def link          = start.link
  def unlink        = ExecutedSpecStart(start.unlink, location, stats)

  def specName = start.specName
  def name = start.name
  def args = start.arguments
  override def toString = "ExecutedSpecStart("+specName+(if (isLink) ","+start.linkToString else "")+")"

}
case class ExecutedSpecEnd(end: SpecEnd, location: Location = new Location, stats: Stats = Stats()) extends ExecutedFragment {
  def specName = end.specName
  def name = end.name
  def title = end.title
  
  override def toString = "ExecutedSpecEnd("+name+")"
}

/**
 * This executed Fragment is used when no text must be displayed (for the successful
 * execution of an Action for example)
 */
case class ExecutedNoText(timer: SimpleTimer = new SimpleTimer, location: Location = new Location) extends ExecutedFragment { outer =>
  def stats: Stats = Stats(timer=outer.timer)
}

import org.specs2.internal.scalaz._
private[specs2]
trait ExecutedFragmentsShow {
  implicit object showExecutedFragments extends Show[ExecutedFragment] {
	  def show(f: ExecutedFragment) = f.toString.toList
  }
}
private[specs2]
object ExecutedFragmentsShow extends ExecutedFragmentsShow
