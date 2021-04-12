package org.specs2
package specification
package dsl
package mutable

import execute.*
import specification.script.*
import specification.core.*
import specification.create.*
import scala.util.NotGiven

/**
 * Create blocks of examples in a mutable specification
 */
trait BlockDsl extends BlockCreation:

  given ToBlock[Fragment, Fragment] with
    def toBlock(s: String, f: =>Fragment): Fragment =
      addBlock(s, f)

  given ToBlock[Fragments, Fragments] with
    def toBlock(s: String, fs: =>Fragments): Fragments =
      addBlock(s, fs)

  given [R : AsResult]: ToBlock[StepParser[R], Fragment] with
    def toBlock(s: String, parser: =>StepParser[R]): Fragment =
      addExample(parser.strip(s), Execution.executed(parser.run(s).fold(execute.Error.apply, AsResult(_))))

  given [R : AsExecution]: ToBlock[R, Fragment] with
    def toBlock(s: String, r: =>R): Fragment =
      addExample(s, r)

  given [R : AsExecution]: ToBlock[String => R, Fragment] with
    def toBlock(s: String, f: =>(String => R)): Fragment =
      addExample(s, f(s))

  extension [S, R](d: String)
    infix def >>(s: =>S)(using t: ToBlock[S, R]): R =
     summon[ToBlock[S, R]].toBlock(d, s)

/**
 * Additional syntax for blocks
 */
trait ExtendedBlockDsl extends BlockDsl:

  extension [S, R] (d: String)(using not: NotGiven[NoExtendedBlockDsl])
    infix def should(s: =>S)(using t: ToBlock[S, R]): R =
      summon[ToBlock[S, R]].toBlock(s"$d should", s)

    infix def can(s: =>S)(using t: ToBlock[S, R]): R =
      summon[ToBlock[S, R]].toBlock(s"$d can", s)

    infix def in(s: =>S)(using t: ToBlock[S, R]): R =
      summon[ToBlock[S, R]].toBlock(d, s)

  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String):
  WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String):
    def apply[T : AsResult](r: =>T): Fragment = ???

private[specs2]
trait BlockCreation extends FragmentBuilder with FragmentsFactory:

  trait ToBlock[S, R]:
    def toBlock(d: String, s: =>S): R

  private val factory = fragmentFactory

  private[specs2] def addBlock[T](text: String, t: =>T, location: StacktraceLocation = StacktraceLocation()): T =
    addStart
    if hasSectionsForBlocks then addFragment(factory.section(text))
    addText(text, location)
    addFragment(factory.tab)
    addBreak
    val result = t
    addFragment(factory.backtab)
    if hasSectionsForBlocks then addFragment(factory.section(text))
    addEnd
    result

  protected[specs2] def addExample[R : AsExecution](d: String, r: =>R) =
    addFragment(fragmentFactory.example(Text(d), summon[AsExecution[R]].execute(r)))
    addFragment(fragmentFactory.break)

  private def addText(text: String, location: StacktraceLocation) =
    addFragment(factory.text(text).setLocation(location))

  private def addBreak = addFragment(factory.break)
  private def addStart = addFragment(factory.start)
  private def addEnd   = addFragment(factory.end)

/** deactivate the BlockDsl implicits */
trait NoBlockDsl:
  self: BlockDsl =>
  given NoBlockDsl = ???

/** deactivate the ExtendedBlockDsl implicits */
trait NoExtendedBlockDsl:
  self: ExtendedBlockDsl =>
  given NoExtendedBlockDsl = ???
