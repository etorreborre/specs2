package org.specs2
package specification
package dsl
package mutable

import execute._
import specification.script._
import specification.core._
import specification.create._
import scala.util.Not

/**
 * Create blocks of examples in a mutable specification
 */
trait BlockDsl extends BlockCreation:

  given ToBlock[Fragment, Fragment]:
    def toBlock(s: String, f: =>Fragment): Fragment =
      addBlock(s, f)

  given ToBlock[Fragments, Fragments]:
    def toBlock(s: String, fs: =>Fragments): Fragments =
      addBlock(s, fs)

  given [R : AsResult] as ToBlock[StepParser[R], Fragment]:
    def toBlock(s: String, parser: =>StepParser[R]): Fragment =
      addExample(parser.strip(s), Execution.executed(parser.run(s).fold(execute.Error.apply, AsResult(_))))

  given [R : AsExecution] as ToBlock[R, Fragment]:
    def toBlock(s: String, r: =>R): Fragment =
      addExample(s, r)

  given [R : AsExecution] as ToBlock[String => R, Fragment]:
    def toBlock(s: String, f: =>(String => R)): Fragment =
      addExample(s, f(s))

  extension [S, R] (d: String)(using not: Not[NoBlockDsl]):
    def >>(s: =>S)(using t: ToBlock[S, R]): R =
     summon[ToBlock[S, R]].toBlock(d, s)

/**
 * Additional syntax for blocks
 */
trait ExtendedBlockDsl extends BlockDsl:

  extension [S, R] (d: String)(using not: Not[NoExtendedBlockDsl]):
    def should(s: =>S)(using t: ToBlock[S, R]): R =
      summon[ToBlock[S, R]].toBlock(s"$d should", s)

    def can(s: =>S)(using t: ToBlock[S, R]): R =
      summon[ToBlock[S, R]].toBlock(s"$d can", s)

    def in(s: =>S)(using t: ToBlock[S, R]): R =
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
