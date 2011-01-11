package org.specs2
package specification

import main.Arguments
import text._
import time.SimpleTimer
import execute._
import main._

/**
 * The Executed Fragments represent pieces of a Specification
 * which have already been executed to provide a Result
 */
sealed trait ExecutedFragment
case class ExecutedText(text: String) extends ExecutedFragment
case class ExecutedResult(s: MarkupString, result: Result, timer: SimpleTimer) extends ExecutedFragment {
  def text(implicit args: Arguments) = s match {
    case CodeMarkup(s) if (!result.expected.isEmpty && !args.fromSource) => CodeMarkup(result.expected)
    case _                                                               => s
  }
}
case class ExecutedBr() extends ExecutedFragment
case class ExecutedEnd() extends ExecutedFragment
case class ExecutedTab(n: Int = 1) extends ExecutedFragment
case class ExecutedBacktab(n: Int = 1) extends ExecutedFragment
case class ExecutedSpecStart(name: SpecName, arguments: Arguments) extends ExecutedFragment {
  override def toString = "ExecutedSpecStart("+name.name+")"
}
case class ExecutedSpecEnd(name: SpecName) extends ExecutedFragment {
  override def toString = "ExecutedSpecEnd("+name.name+")"
}
case class ExecutedSee(link: HtmlLink) extends ExecutedFragment
/**
 * This executed Fragment is used when no text must be displayed (for the successful
 * execution of an Action for example)
 */
case class ExecutedNoText(timer: SimpleTimer = new SimpleTimer) extends ExecutedFragment
