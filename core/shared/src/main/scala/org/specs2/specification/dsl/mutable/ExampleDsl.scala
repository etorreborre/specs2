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
trait ExampleDsl extends ExampleDsl1 with dsl.ExampleDsl:

  extension (d: String)(using not: Not[NoBangExamples])
    override def !(execution: Execution): Fragment =
      addFragment(fragmentFactory.example(Text(d), execution))

  extension [R : AsResult](d: String)(using not: Not[NoBangExamples])
    override def !(r: => R): Fragment =
      addFragment(fragmentFactory.example(d, r))

    override def !(r: String => R): Fragment =
      addFragment(fragmentFactory.example(d, r))

  extension [R : AsResult](d: String)(using not: Not[NoBangExamples])
    override def !(r: Env => R)(using p: ImplicitParam): Fragment =
      addFragment(fragmentFactory.example(d, r))

private[specs2]
trait ExampleDsl1 extends BlockDsl with ExampleDsl0:

  extension [R: AsExecution](d: String)(using not: Not[NoExampleDsl]):
    def >>(f: String => R): Fragment =
      d.>>(summon[AsExecution[R]].execute(f(d)))

    def in(f: String => R): Fragment =
      d.>>(f)(summon[AsExecution[R]])

  extension [R: AsResult](d: String)(using not: Not[NoExampleDsl]):
    def in(parser: StepParser[R]): Fragment =
      d.>>(parser)

    def >>(parser: StepParser[R]): Fragment =
      addFragment(
        fragmentFactory.example(Text(parser.strip(d)),
                                Execution.executed(parser.run(d).fold(execute.Error.apply, AsResult(_)))))
      addFragment(fragmentFactory.break)

  extension (d: String)(using not: Not[NoExampleDsl]):
    def >>(execution: Execution): Fragment =
      addExample(d, execution)

    def in(f: =>Fragment): Fragment =
      describe(d) >> f

    def in(fs: =>Fragments)(using p1: ImplicitParam1): Fragments =
      describe(d).>>(fs)

    def in(execution: Execution): Fragment =
      d >> execution

  private[specs2] def addExample(d: String, execution: Execution) =
    addFragment(fragmentFactory.example(Text(d), execution))
    addFragment(fragmentFactory.break)

end ExampleDsl1

/**
 * Lightweight ExampleDsl trait
 */
private[specs2]
trait ExampleDsl0 extends BlockCreation:

  extension (d: String)(using not: Not[NoExampleDsl])
    def >>(f: =>Fragment): Fragment =
      addBlock(d, f)

    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(d, fs) }

    def should(f: => Fragment): Fragment =
      addBlock(s"$d should", f)

    def should(fs: => Fragments)(using p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d should", fs) }

    def can(fs: => Fragments)(using p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d can", fs) }

    def can(f: => Fragment): Fragment =
      addBlock(s"$d can", f)

  extension [R : AsExecution](d: String)(using not: Not[NoExampleDsl])
    def >>(r: =>R): Fragment =
      addFragment(fragmentFactory.example(Text(d), summon[AsExecution[R]].execute(r)))
      addFragment(fragmentFactory.break)

    def in(r: =>R): Fragment = d >> r

/** deactivate the ExampleDsl implicits */
trait NoExampleDsl { self: ExampleDsl =>
  given NoExampleDsl = ???
}
