package org.specs2
package specification
package dsl
package mutable

import org.specs2.execute.AsResult
import control.{ImplicitParameters, Use}
import ImplicitParameters._
import org.specs2.specification.core._
import org.specs2.specification.script.StepParser

/**
 * Dsl for creating examples in a mutable specification
 */
trait ExampleDsl extends ExampleDsl1 with dsl.ExampleDsl {

  override implicit def bangExample(d: String): BangExample =
    new MutableBangExample(d)

  class MutableBangExample(d: String) extends BangExample(d) {
    override def !(execution: Execution): Fragment                                       = addFragment(fragmentFactory.example(Text(d), execution))
    override def ![R : AsResult](r: => R): Fragment                                      = addFragment(fragmentFactory.example(d, r))
    override def ![R : AsResult](r: String => R): Fragment                               = addFragment(fragmentFactory.example(d, r))
    override def ![R](r: Env => R)(implicit as: AsResult[R], p: ImplicitParam): Fragment = addFragment(fragmentFactory.example(d, r)(as, p))
  }
}

private[specs2]
trait ExampleDsl1 extends BlockDsl with ExampleDsl0 {

  implicit def blockExample1(d: String) = new BlockExample1(d)

  class BlockExample1(d: String) extends BlockExample0(d) {
    def >>[R](f: String => R)(implicit asExecution: AsExecution[R]): Fragment =
      >>(asExecution.execute(f(d)))

    def >>(execution: =>Execution): Fragment = {
      addFragment(fragmentFactory.example(Text(d), Execution.withEnvFlatten(_ => execution)))
      addFragment(fragmentFactory.break)
    }

    def >>[R: AsResult](parser: StepParser[R]): Fragment = {
      addFragment(
        fragmentFactory.example(Text(parser.strip(d)),
                                Execution.executed(parser.run(d).fold(execute.Error.apply, AsResult(_)))))
      addFragment(fragmentFactory.break)
    }

    def in[R](f: String => R)(implicit ar: AsExecution[R]): Fragment =
      >>(f)(ar)

    def in(f: =>Fragment): Fragment =
      describe(d) >> f

    def in(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
      describe(d).>>(fs)(p1)

    def in[R: AsResult](parser: StepParser[R]): Fragment =
      d.>>(parser)

    def in(execution: Execution): Fragment =
      d >> execution
  }
}

/**
 * Lightweight ExampleDsl trait
 */
private[specs2]
trait ExampleDsl0 extends BlockCreation {
  private[specs2] def addExecution(d: String, execution: =>Execution): Fragment = {
    addFragment(fragmentFactory.example(Text(d), execution))
    addFragment(fragmentFactory.break)
  }

  implicit def blockExample0(d: String): BlockExample0 =
    new BlockExample0(d)

  class BlockExample0(d: String) {

    def >>[R : AsExecution](r: =>R)(implicit p1: ImplicitParam1): Fragment =
      Use.ignoring(p1) { addExecution(d, AsExecution.apply[R].execute(r)) }

    def in[R : AsExecution](r: =>R): Fragment = d >> r

    def >>(f: =>Fragment)(implicit p1: ImplicitParam1): Fragment =
      Use.ignoring(p1) { addBlock(d, f, addFragmentBlock) }

    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(d, fs, addFragmentsBlock) }

    def should(f: => Fragment): Fragment =
      addBlock(s"$d should", f, addFragmentBlock)

    def should(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d should", fs, addFragmentsBlock) }

    def can(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d can", fs, addFragmentsBlock) }

    def can(f: => Fragment): Fragment =
      addBlock(s"$d can", f, addFragmentBlock)
  }
}

/** deactivate the ExampleDsl implicits */
trait NoExampleDsl extends ExampleDsl {
  override def blockExample0(d: String) = super.blockExample0(d)
}
