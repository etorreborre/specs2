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
  /** @return the statistics for the execution of a Fragment */
  def stats = Stats()
}

private[specs2]
object ExecutedFragments {
  /** @return true if the ExecutedFragment is a Text */
  def isExecutedText: Function[ExecutedFragment, Boolean] = { case ExecutedText(_, _) => true; case _ => false }
  /** @return the text if the Fragment is a Text */
  def isSomeExecutedText: PartialFunction[ExecutedFragment, ExecutedText] = { case t @ ExecutedText(_, _) => t }
  /** @return true if the ExecutedFragment is a result */
  def isExecutedResult: Function[ExecutedFragment, Boolean] = { case ExecutedResult(_, _, _, _) => true; case _ => false }
  /** @return true if the ExecutedFragment is a Start */
  def isExecutedSpecStart: Function[ExecutedFragment, Boolean] = { case ExecutedSpecStart(_, _, _) => true; case _ => false }
  /** @return true if the ExecutedFragment is an End */
  def isExecutedSpecEnd: Function[ExecutedFragment, Boolean] = { case ExecutedSpecEnd(_, _) => true; case _ => false }
}

case class ExecutedText(text: String, location: Location) extends ExecutedFragment
case class ExecutedResult(s: MarkupString, result: Result, timer: SimpleTimer, location: Location) extends ExecutedFragment {
  
  override def stats = Stats(result)
  def text(implicit args: Arguments) = s match {
    case CodeMarkup(s) if (!result.expected.isEmpty && !args.fromSource) => CodeMarkup(result.expected)
    case _                                                               => s
  }
}
trait ExecutedStandardFragment
case class ExecutedBr(location: Location) extends ExecutedFragment with ExecutedStandardFragment
case class ExecutedEnd(location: Location) extends ExecutedFragment with ExecutedStandardFragment
case class ExecutedTab(n: Int = 1, location: Location) extends ExecutedFragment with ExecutedStandardFragment
case class ExecutedBacktab(n: Int = 1, location: Location) extends ExecutedFragment with ExecutedStandardFragment

case class ExecutedSpecStart(name: SpecName, arguments: Arguments, location: Location) extends ExecutedFragment {
  override def toString = "ExecutedSpecStart("+name.name+")"
}
case class ExecutedSpecEnd(name: SpecName, location: Location) extends ExecutedFragment {
  override def toString = "ExecutedSpecEnd("+name.name+")"
}

case class ExecutedSee(name: SpecName, link: HtmlLink, seeOnly: Boolean = true, override val stats: Stats = Stats(), location: Location) extends ExecutedFragment
/**
 * This executed Fragment is used when no text must be displayed (for the successful
 * execution of an Action for example)
 */
case class ExecutedNoText(timer: SimpleTimer = new SimpleTimer, location: Location) extends ExecutedFragment

import org.specs2.internal.scalaz._
private[specs2]
trait ExecutedFragmentsShow {
  implicit object showExecutedFragments extends Show[ExecutedFragment] {
	  def show(f: ExecutedFragment) = f.toString.toList
  }
}
private[specs2]
object ExecutedFragmentsShow extends ExecutedFragmentsShow
