package org.specs2
package specification
package dsl
package mutable

import org.specs2.execute.AsResult
import control.{ImplicitParameters, Use}
import ImplicitParameters._
import org.specs2.specification.core._
import org.specs2.specification.script.StepParser
import scala.util.Not

/**
 * Dsl for creating examples in a mutable specification
 */
trait ExtendedExampleDsl extends ExampleDsl1 with dsl.ExtendedExampleDsl:

  extension (d: String)(using not: Not[NoBangExamples]):
    override def !(execution: Execution): Fragment =
      addFragment(fragmentFactory.example(Text(d), execution))

  extension [R : AsResult](d: String)(using not: Not[NoBangExamples]):
    override def !(r: => R): Fragment =
      addFragment(fragmentFactory.example(d, r))

    override def !(r: String => R): Fragment =
      addFragment(fragmentFactory.example(d, r))

  extension [R : AsResult](d: String)(using not: Not[NoBangExamples]):
    override def !(r: Env => R)(using p: ImplicitParam): Fragment =
      addFragment(fragmentFactory.example(d, r))

private[specs2]
trait ExampleDsl1 extends ExtendedBlockDsl with ExampleDsl:

  implicit class describeExampleIn[R : AsExecution](d: String)(using not: Not[NoExampleDsl]):
    def in(r: =>R): Fragment =
      addExample(d, r)

    def in(f: String => R): Fragment =
      addExample(d, f(d))

  implicit class describeStepIn[R: AsResult](d: String)(using not: Not[NoExampleDsl]):
    def in(parser: StepParser[R]): Fragment =
      d.>>(parser)

/**
 * Lightweight ExtendedExampleDsl trait
 */
private[specs2]
trait ExampleDsl extends BlockCreation:

  implicit class describeExample[R : AsExecution](d: String)(using not: Not[NoExampleDsl]):
    def >>(r: =>R): Fragment =
      addExample(d, r)

    def >>(f: String => R): Fragment =
      addExample(d, f(d))

  implicit class describeStep[R: AsResult](d: String)(using not: Not[NoExampleDsl]):
    def >>(parser: StepParser[R]): Fragment =
      addFragment(
        fragmentFactory.example(Text(parser.strip(d)),
                                Execution.executed(parser.run(d).fold(execute.Error.apply, AsResult(_)))))
      addFragment(fragmentFactory.break)

  protected[specs2] def addExample[R : AsExecution](d: String, r: R) =
    addFragment(fragmentFactory.example(Text(d), summon[AsExecution[R]].execute(r)))
    addFragment(fragmentFactory.break)

/** deactivate the ExtendedExampleDsl implicits */
trait NoExampleDsl:
  self: ExtendedExampleDsl =>
  given NoExampleDsl = ???
