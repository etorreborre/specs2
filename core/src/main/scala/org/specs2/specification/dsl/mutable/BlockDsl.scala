package org.specs2
package specification
package dsl
package mutable

import control.ImplicitParameters._
import execute.AsResult
import org.specs2.specification.core.{StacktraceLocation, Fragment}
import specification.create.FragmentsFactory

trait BlockDsl extends FragmentBuilder with FragmentsFactory {
  private val factory = fragmentFactory
  
  implicit class describe(d: String) {
    def >>(f: =>Fragment): Unit = addBlock(d, f, StacktraceLocation())
    def should(f: =>Fragment)   = addBlock(s"$d should", f, StacktraceLocation())
    def can(f: =>Fragment)      = addBlock(s"$d can", f, StacktraceLocation())

    def >>(f: =>Unit)(implicit p: ImplicitParam): Unit = addBlock(d, f, StacktraceLocation())
    def should(f: =>Unit)(implicit p: ImplicitParam)   = addBlock(s"$d should", f, StacktraceLocation())
    def can(f: =>Unit)(implicit p: ImplicitParam)      = addBlock(s"$d can", f, StacktraceLocation())

    private def addBlock(text: String, f: =>Any, location: StacktraceLocation) = addFragmentBlock {
      addStart
      addBreak
      addText(text, location)
      addFragment(factory.tab)
      addBreak
      addFragmentBlock(f)
      addFragment(factory.backtab)
      addEnd
    }

    private def addText(text: String, location: StacktraceLocation) =
      addFragment(factory.text(text).setLocation(location))

    private def addBreak = addFragment(factory.break)
    private def addStart = addFragment(factory.start)
    private def addEnd = addFragment(factory.end)
  }

  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String):
  WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String) {
    def apply[T : AsResult](r: =>T): Fragment = ???
  }

}


