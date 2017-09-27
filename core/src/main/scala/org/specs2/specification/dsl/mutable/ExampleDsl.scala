package org.specs2
package specification
package dsl
package mutable

import java.util.concurrent.ExecutorService

import org.specs2.concurrent.ExecutionEnv
import org.specs2.execute.AsResult
import control.{ImplicitParameters, Use}
import ImplicitParameters._
import org.specs2.main.CommandLine
import org.specs2.specification.core._
import org.specs2.specification.script.StepParser

import scala.concurrent.ExecutionContext

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
  // deactivate block0
  override def blockExample0(d: String) = super.blockExample0(d)

  implicit def blockExample(d: String) = new BlockExample(d)

  class BlockExample(d: String) extends BlockExample0(d) {
    def >>[R](f: String => R)(implicit asResult: AsResult[R], p1: ImplicitParam1, p2: ImplicitParam2): Fragment =
      Use.ignoring(p1, p2) { >>(Execution.result(f(d))) }

    def >>[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment =
      Use.ignoring(p) { >>(Execution.withEnv((env: Env) => asResult.asResult(f(env.arguments.commandLine)))) }

    def >>[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment =
      Use.ignoring(p1) { >>(Execution.withEnv((env: Env) => asResult.asResult(f(env)))) }

    def >>[R](f: ExecutionContext => R)(implicit asResult: AsResult[R], p2: ImplicitParam2): Fragment =
      Use.ignoring(p2) { >>(Execution.withExecutionContext(f)) }

    def >>[R](f: ExecutionEnv => R)(implicit asResult: AsResult[R], p3: ImplicitParam3): Fragment =
      Use.ignoring(p3) { >>(Execution.withExecutionEnv(f)) }

    def >>[R](f: ExecutorService => R)(implicit asResult: AsResult[R], p4: ImplicitParam4): Fragment =
      Use.ignoring(p4) { >>(Execution.withExecutorService(f)) }

    def >>(execution: Execution): Fragment = {
      addFragment(fragmentFactory.example(Text(d), execution))
      addFragment(fragmentFactory.break)
    }

    def >>[R: AsResult](parser: StepParser[R]): Fragment = {
      addFragment(
        fragmentFactory.example(Text(parser.strip(d)),
                                Execution.executed(parser.run(d).fold(execute.Error.apply, AsResult(_)))))
      addFragment(fragmentFactory.break)
    }

    def in[R](f: String => R)(implicit ar: AsResult[R], p1: ImplicitParam1, p2: ImplicitParam2): Fragment =
      >>(f)(ar, p1, p2)

    def in(f: =>Fragment): Fragment =
      describe(d) >> f

    def in(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
      describe(d).>>(fs)(p1)

    def in[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment =
      >>(f)(asResult, p)

    def in[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment =
      d.>>(f)(asResult, p1)

    def in[R](f: ExecutionContext => R)(implicit asResult: AsResult[R], p2: ImplicitParam2): Fragment =
      d.>>(f)(asResult, p2)

    def in[R](f: ExecutionEnv => R)(implicit asResult: AsResult[R], p3: ImplicitParam3): Fragment =
      d.>>(f)(asResult, p3)

    def in[R](f: ExecutorService => R)(implicit asResult: AsResult[R], p4: ImplicitParam4): Fragment =
      d.>>(f)(asResult, p4)

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
  implicit def blockExample0(d: String): BlockExample0 = new BlockExample0(d)

  class BlockExample0(d: String) {
    def >>(f: =>Fragment): Fragment =
      addBlock(d, f, addFragmentBlock)

    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(d, fs, addFragmentsBlock) }

    def >>[R : AsExecution](r: =>R): Fragment = {
      addFragment(fragmentFactory.example(Text(d), AsExecution.apply[R].execute(r)))
      addFragment(fragmentFactory.break)
    }

    def should(f: => Fragment): Fragment =
      addBlock(s"$d should", f, addFragmentBlock)

    def should(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d should", fs, addFragmentsBlock) }

    def can(fs: => Fragments)(implicit p1: ImplicitParam1): Fragments =
      Use.ignoring(p1) { addBlock(s"$d can", fs, addFragmentsBlock) }

    def can(f: => Fragment): Fragment =
      addBlock(s"$d can", f, addFragmentBlock)

    def in[R : AsResult](r: =>R): Fragment = d >> r
  }
}

/** deactivate the ExampleDsl implicits */
trait NoExampleDsl extends ExampleDsl {
  override def blockExample(d: String) = super.blockExample(d)
}


