package org.specs2
package specification
package dsl
package mutable

import control.ImplicitParameters._
import execute.AsResult
import org.specs2.specification.core.{Fragments, StacktraceLocation, Fragment}
import specification.create.FragmentsFactory

trait BlockDsl extends FragmentBuilder with FragmentsFactory {
  private val factory = fragmentFactory
  
  implicit class describe(d: String) {
    def >>(f: =>Fragment): Fragment     = addBlock(d,            f, addFragmentBlock, StacktraceLocation())
    def should(f: =>Fragment): Fragment = addBlock(s"$d should", f, addFragmentBlock, StacktraceLocation())
    def can(f: =>Fragment): Fragment    = addBlock(s"$d can",    f, addFragmentBlock, StacktraceLocation())

    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments     = addBlock(d,            fs, addFragmentsBlock, StacktraceLocation())
    def should(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments = addBlock(s"$d should", fs, addFragmentsBlock, StacktraceLocation())
    def can(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments    = addBlock(s"$d can",    fs, addFragmentsBlock, StacktraceLocation())

    private def addBlock[T](text: String, t: =>T, addFunction: (=>T) => T, location: StacktraceLocation): T = addFunction {
      addStart
      addBreak
      addText(text, location)
      addFragment(factory.tab)
      addBreak
      val result = addFunction(t)
      addFragment(factory.backtab)
      addEnd
      result
    }

    private def addText(text: String, location: StacktraceLocation) =
      addFragment(factory.text(text).setLocation(location))

    private def addBreak = addFragment(factory.break)
    private def addStart = addFragment(factory.start)
    private def addEnd   = addFragment(factory.end)
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


