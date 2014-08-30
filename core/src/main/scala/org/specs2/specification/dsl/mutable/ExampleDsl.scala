package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters
import ImplicitParameters._
import org.specs2.main.CommandLine
import org.specs2.specification.core._
import specification.dsl

trait ExampleDsl extends ExampleDsl1 with dsl.ExampleDsl {
  override implicit def bangExample(d: String): BangExample =
    new MutableBangExample(d)

  class MutableBangExample(d: String) extends BangExample(d) {
    override def ![R : AsResult](r: => R): Fragment                                      = addFragment(fragmentFactory.example(d, r))
    override def ![R : AsResult](r: String => R): Fragment                               = addFragment(fragmentFactory.example(d, r))
    override def ![R](r: Env => R)(implicit as: AsResult[R], p: ImplicitParam): Fragment = addFragment(fragmentFactory.example(d, r)(as, p))
  }
}

private[specs2]
trait ExampleDsl1 extends BlockDsl {
  implicit def blockExample(d: String) = new BlockExample(d)

  class BlockExample(d: String) {
    def >>(f: =>Fragment): Fragment = describe(d) >> f
    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments = describe(d).>>(fs)(p1)

    def >>[R : AsResult](r: =>R): Fragment =
      >>(Execution.result(r))

    def >>[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment =
      >>(Execution.withEnv((env: Env) => asResult.asResult(f(env.arguments.commandLine))))

    def >>[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment =
      >>(Execution.withEnv((env: Env) => asResult.asResult(f(env))))

    def >>(execution: Execution): Fragment = {
      addFragment(fragmentFactory.example(Text(d), execution))
      addFragment(fragmentFactory.break)
    }

    def in[R : AsResult](r: =>R): Fragment = d >> r
    def in[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment = d.>>(f)(asResult, p)
    def in[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment = d.>>(f)(asResult, p1)
    def in(execution: Execution): Fragment = d >> execution
  }
}

/**
 * Lightweight ExampleDsl trait
 */
private[specs2]
trait ExampleDsl0 extends BlockCreation {
  implicit def blockExample(d: String) = new BlockExample(d)

  class BlockExample(d: String) {
    def >>(f: =>Fragment): Fragment = addBlock(d, f, addFragmentBlock)

    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments = addBlock(d, fs, addFragmentsBlock)

    def >>[R : AsResult](r: =>R): Fragment = {
      addFragment(fragmentFactory.example(Text(d), Execution.result(r)))
      addFragment(fragmentFactory.break)
    }

    def in[R : AsResult](r: =>R): Fragment = d >> r
  }
}

trait NoBlockExample extends ExampleDsl {
  override def blockExample(d: String) = super.blockExample(d)
}


