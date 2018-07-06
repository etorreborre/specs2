package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import org.specs2.control.Use
import org.specs2.specification.core.{Fragment, Fragments, StacktraceLocation}
import specification.create.FragmentsFactory
import org.specs2.control.ImplicitParameters._

/**
 * Create blocks of examples in a mutable specification
 */
trait BlockDsl extends BlockCreation {
  implicit class describe(d: String) {
    def >>(f: => Fragment): Fragment     = addFragmentBlockWithText(d, f)
    def should(f: => Fragment): Fragment = addFragmentBlockWithText(s"$d should", f)
    def can(f: => Fragment): Fragment    = addFragmentBlockWithText(s"$d can", f)

    def >>(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      addFragmentsBlockWithText(d, fs)(p1)

    def should(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      addFragmentsBlockWithText(s"$d should", fs)(p1)

    def can(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      addFragmentsBlockWithText(s"$d can", fs)(p1)
  }

  def addFragmentBlockWithText(text: String, f: =>Fragment): Fragment =
    addBlock(text, f, addFragmentBlock)

  def addFragmentsBlockWithText(text: String, fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
    Use.ignoring(p1)(addBlock(text, fs, addFragmentsBlock))

  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String):
  WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String) {
    def apply[T : AsResult](r: =>T): Fragment = ???
  }
}

private[specs2]
trait BlockCreation extends FragmentBuilder with FragmentsFactory {
  private val factory = fragmentFactory

  private[specs2] def addBlock[T](text: String, t: =>T, addFunction: (=>T) => T, location: StacktraceLocation = StacktraceLocation()): T = addFunction {

    addStart
    if (hasSectionsForBlocks) addFragment(factory.section(text))
    addText(text, location)
    addFragment(factory.tab)
    addBreak
    val result = addFunction(t)
    addFragment(factory.backtab)
    if (hasSectionsForBlocks) addFragment(factory.section(text))
    addEnd
    result


  }

  private def addText(text: String, location: StacktraceLocation) =
    addFragment(factory.text(text).setLocation(location))

  private def addBreak = addFragment(factory.break)
  private def addStart = addFragment(factory.start)
  private def addEnd   = addFragment(factory.end)

}


