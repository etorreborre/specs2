package org.specs2
package specification

import execute._

/**
 * The Executed Fragments represent pieces of a Specification
 * which have already been executed to provide a Result
 */
sealed trait ExecutedFragment
case class ExecutedText(text: String) extends ExecutedFragment
case class ExecutedResult(text: String, result: Result) extends ExecutedFragment
case class ExecutedBr() extends ExecutedFragment
case class ExecutedPar() extends ExecutedFragment
case class ExecutedEnd() extends ExecutedFragment
case class ExecutedSpecStart(name: String) extends ExecutedFragment
case class ExecutedSpecEnd(name: String) extends ExecutedFragment
/** 
 * This executed Fragment is used when no text must be displayed (for the successful
 * execution of an Action for example)
 */
case class ExecutedNoText() extends ExecutedFragment
