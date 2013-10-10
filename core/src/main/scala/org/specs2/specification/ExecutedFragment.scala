package org.specs2
package specification

import text._
import time.SimpleTimer
import execute._
import main._
import io.Location
import scalaz.concurrent.Promise

/**
 * The Executing Fragment trait represent a Fragment being executed.
 *
 * Some fragments are executed right away, other ones on demand synchronously, and other ones concurrently
 */
sealed trait ExecutingFragment {
  /** @return the fragment, before execution */
  def original: Fragment
  /** @return a fragment, completely executed */
  def get: ExecutedFragment
  /** @return and executing fragment, with another embedded ExecutedFragment */
  def map(f: ExecutedFragment => ExecutedFragment): ExecutingFragment
}

/**
 * The Executed Fragments represent pieces of a Specification
 * which have already been executed to provide a Result
 */
sealed trait ExecutedFragment {
  /** @return the fragment before execution */
  def original: Fragment
  /** @return the location of the executed Fragment */
  def location: Location
  /** @return the statistics of the executed Fragment */
  def stats: Stats
  /** @return wait for this fragment to be completely executed */
  def get: ExecutedFragment = this
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
  /** @return true if the ExecutedFragment is a Start */
  def isSomeExecutedSpecStart: PartialFunction[ExecutedFragment, ExecutedSpecStart] = { case s @ ExecutedSpecStart(_,_,_) => s }
  /** @return true if the ExecutedFragment is an End */
  def isSomeExecutedSpecEnd: PartialFunction[ExecutedFragment, ExecutedSpecEnd] = { case s @ ExecutedSpecEnd(_,_,_) => s }
  /** @return true if the ExecutedFragment is an End */
  def isExecutedSpecEnd: Function[ExecutedFragment, Boolean] = { case ExecutedSpecEnd(_,_,_) => true; case _ => false }
  /** @return true if the ExecutedFragment is a start with a link */
  def isIncludeLink: PartialFunction[ExecutedFragment, ExecutedSpecStart] = { case s @ ExecutedSpecStart(_,_,_) if s.isIncludeLink => s }
  /** @return true if the ExecutedFragment is a start with a see only link */
  def isSeeOnlyLink: PartialFunction[ExecutedFragment, ExecutedSpecStart] =  { case s @ ExecutedSpecStart(_,_,_) if s.isSeeOnlyLink => s }
  /** @return true if the executed fragment is not a Failure or an Error */
  def isOk = (e: ExecutedFragment) => e match {
    case ExecutedResult(_,r,_,_,_) if r.isFailure || r.isError => false
    case other                                                 => true
  }
  /** @return true if the executed fragment is skipped */
  def isSkipped = (e: ExecutedFragment) => e match {
    case ExecutedResult(_,r,_,_,_) if r.isSkipped => true
    case other                                    => false
  }

}

case class ExecutedText(textFragment: Text, location: Location = new Location) extends ExecutedFragment {
  def original = textFragment

  def formattedString = textFragment.text
  def text = textFragment.t
  def stats: Stats = Stats()
}
object ExecutedText1 {
  def unapply(executedText: ExecutedText) = Option(executedText.text)
}
case class ExecutedResult(s: FormattedString, result: Result, timer: SimpleTimer, location: Location, statistics: Stats) extends ExecutedFragment { outer =>
  def original = Example(s, result)

  def text(implicit args: Arguments) =
    if (s.formatting.markdown && !result.expected.isEmpty && !args.fromSource) s.map(_ => result.expected)
    else s

  def hasDescription = !s.isEmpty

  def stats = statistics.copy(timer = outer.timer)
  def message = result.message

  def isSuccess   = stats.isSuccess
  def isError     = stats.hasErrors
  def isFailure   = stats.hasFailures
  def isIssue     = stats.hasIssues
  def isSuspended = stats.hasSuspended
}
private[specs2]
object ExecutedResult {
  def apply(desc: String, r: Result): ExecutedResult = ExecutedResult(FormattedString(desc), r, new SimpleTimer, new Location, Stats())
  def apply(r: Result): ExecutedResult = ExecutedResult(FormattedString.empty, r, new SimpleTimer, new Location, Stats())
}

trait ExecutedStandardFragment extends ExecutedFragment {
  val stats: Stats = Stats()
}
case class ExecutedBr(location: Location = new Location) extends ExecutedStandardFragment {
  def original = StandardFragments.Br()
}
case class ExecutedEnd(location: Location = new Location) extends ExecutedStandardFragment {
  def original = StandardFragments.End()
}

case class ExecutedTab(n: Int = 1, location: Location = new Location) extends ExecutedStandardFragment {
  def original = StandardFragments.Backtab()
}
case class ExecutedBacktab(n: Int = 1, location: Location = new Location) extends ExecutedStandardFragment {
  def original = StandardFragments.Backtab()
}

case class ExecutedSpecStart(start: SpecStart, location: Location = new Location, stats: Stats = Stats().startTimer) extends ExecutedFragment {
  def original = start

  def isSeeOnlyLink = start.isSeeOnlyLink
  def isIncludeLink = start.isIncludeLink
  def isLink        = start.isLink
  def link          = start.link
  def hidden        = start.hidden
  def unlink        = ExecutedSpecStart(start.unlink, location, stats)

  def specName = start.specName
  def name = start.name
  def title = start.title
  def args = start.arguments
  override def toString = "ExecutedSpecStart("+specName+(if (isLink) ","+start.linkToString else "")+")"

}
case class ExecutedSpecEnd(end: SpecEnd, location: Location = new Location, stats: Stats = Stats()) extends ExecutedFragment {
  def original = end

  def specName = end.specName
  def name = end.name
  def title = end.title
  def isSeeOnlyLink = end.isSeeOnlyLink
  
  override def toString = "ExecutedSpecEnd("+name+")"
}

/**
 * This executed Fragment is used when no text must be displayed (for the successful
 * execution of an Action for example)
 */
case class ExecutedNoText(original: Fragment,
                          timer: SimpleTimer = new SimpleTimer,
                          location: Location = new Location) extends ExecutedFragment { outer =>
  def isAction = original match {
    case _: Action => true
    case _         => false
  }

  def stats: Stats = Stats(timer=outer.timer)
}

/**
 * embed an already executed Fragment
 */
case class FinishedExecutingFragment(f: ExecutedFragment) extends ExecutingFragment {
  def original: Fragment = f.original
  def get = f
  def map(function: ExecutedFragment => ExecutedFragment) = FinishedExecutingFragment(function(f))
}

/**
 * embed an executed Fragment into a promise
 */
case class PromisedExecutingFragment(promised: Promise[ExecutedFragment], original: Fragment) extends ExecutingFragment {
  def get = promised.get
  def map(function: ExecutedFragment => ExecutedFragment) = copy(promised = promised.map(function))
}

/**
 * embed an executing Fragment into a function to execute it on demand
 */
case class LazyExecutingFragment(f: ()=>ExecutedFragment, original: Fragment) extends ExecutingFragment {
  lazy val get = f()
  def map(function: ExecutedFragment => ExecutedFragment) = LazyExecutingFragment(() =>function(f()), original)
}

import scalaz._
private[specs2]
trait ExecutedFragmentsShow {
  implicit object showExecutedFragments extends Show[ExecutedFragment] {
    override def shows(f: ExecutedFragment) = f.toString
  }
}
private[specs2]
object ExecutedFragmentsShow extends ExecutedFragmentsShow
