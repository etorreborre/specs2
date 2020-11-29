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
trait ExtendedBlockDsl extends BlockDsl:
  implicit class extendedDescribeBlock(d: String):
    def should(f: =>Fragment): Fragment =
      addBlock(s"$d should", f)

    def can(f: =>Fragment): Fragment =
      addBlock(s"$d can", f)

    def should(fs: =>Fragments)(using p1: ImplicitParam1): Fragments =
      addBlock(s"$d should", fs)

    def can(fs: =>Fragments)(using p1: ImplicitParam1): Fragments =
      addBlock(s"$d can", fs)

    def in(f: =>Fragment): Fragment =
      addBlock(d, f)

    def in(fs: =>Fragments)(using p1: ImplicitParam1): Fragments =
      addBlock(d, fs)

  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String):
  WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String):
    def apply[T : AsResult](r: =>T): Fragment = ???

trait BlockDsl extends BlockCreation:
  implicit class describeBlock(d: String):
    def >>(f: =>Fragment): Fragment =
      addBlock(d, f)

    def >>(fs: =>Fragments)(using p1: ImplicitParam1): Fragments =
      addBlock(d, fs)

private[specs2]
trait BlockCreation extends FragmentBuilder with FragmentsFactory:
  private val factory = fragmentFactory

  private[specs2] def addBlock[T](text: String, t: =>T, location: StacktraceLocation = StacktraceLocation()): T =
    addStart
    if hasSectionsForBlocks then addFragment(factory.section(text))
    //print((text, location.trace.map(t => println((text, t)))))
    addText(text, location)
    addFragment(factory.tab)
    addBreak
    val result = t
    addFragment(factory.backtab)
    if hasSectionsForBlocks then addFragment(factory.section(text))
    addEnd
    result

  private def addText(text: String, location: StacktraceLocation) =
    addFragment(factory.text(text).setLocation(location))

  private def addBreak = addFragment(factory.break)
  private def addStart = addFragment(factory.start)
  private def addEnd   = addFragment(factory.end)
