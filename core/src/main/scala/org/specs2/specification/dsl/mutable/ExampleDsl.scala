package org.specs2
package specification
package dsl
package mutable

import java.util.concurrent.ExecutorService

import org.specs2.concurrent.ExecutionEnv
import org.specs2.execute.AsResult
import control.ImplicitParameters
import ImplicitParameters._
import org.specs2.main.{CommandLineAsResult, CommandLine}
import org.specs2.specification.core._
import specification.dsl

import scala.concurrent.ExecutionContext

/**
 * Dsl for creating examples in a mutable specification
 */
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
    def >>[R : CommandLineAsResult](r: =>R): Fragment =
      >>(Execution.withEnv((env: Env) => CommandLineAsResult(r).apply(env.arguments.commandLine)))

    def >>(f: =>Fragment): Fragment = describe(d) >> f
    def >>(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments = describe(d).>>(fs)(p1)

    def >>[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment =
      >>(Execution.withEnv((env: Env) => asResult.asResult(f(env.arguments.commandLine))))

    def >>[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment =
      >>(Execution.withEnv((env: Env) => asResult.asResult(f(env))))

    def >>[R](f: ExecutionContext => R)(implicit asResult: AsResult[R], p2: ImplicitParam2): Fragment =
      >>(Execution.withExecutionContext(f))

    def >>[R](f: ExecutionEnv => R)(implicit asResult: AsResult[R], p3: ImplicitParam3): Fragment =
      >>(Execution.withExecutionEnv(f))

    def >>[R](f: ExecutorService => R)(implicit asResult: AsResult[R], p4: ImplicitParam4): Fragment =
      >>(Execution.withExecutorService(f))

    def >>(execution: Execution): Fragment = {
      addFragment(fragmentFactory.example(Text(d), execution))
      addFragment(fragmentFactory.break)
    }

    def in[R : CommandLineAsResult](r: =>R): Fragment = >>(r)
    def in(f: =>Fragment): Fragment = describe(d) >> f
    def in(fs: =>Fragments)(implicit p1: ImplicitParam1): Fragments = describe(d).>>(fs)(p1)

    def in[R](f: CommandLine => R)(implicit asResult: AsResult[R], p: ImplicitParam): Fragment = >>(f)(asResult, p)
    def in[R](f: Env => R)(implicit asResult: AsResult[R], p1: ImplicitParam1): Fragment = d.>>(f)(asResult, p1)
    def in[R](f: ExecutionContext => R)(implicit asResult: AsResult[R], p2: ImplicitParam2): Fragment = d.>>(f)(asResult, p2)
    def in[R](f: ExecutionEnv => R)(implicit asResult: AsResult[R], p3: ImplicitParam3): Fragment = d.>>(f)(asResult, p3)
    def in[R](f: ExecutorService => R)(implicit asResult: AsResult[R], p4: ImplicitParam4): Fragment = d.>>(f)(asResult, p4)
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

    def should(f: => Fragment): Fragment = addBlock(s"$d should", f, addFragmentBlock)
    def can(f: => Fragment): Fragment    = addBlock(s"$d can",    f, addFragmentBlock)

    def in[R : AsResult](r: =>R): Fragment = d >> r
  }
}

trait NoBlockExample extends ExampleDsl {
  override def blockExample(d: String) = super.blockExample(d)
}


