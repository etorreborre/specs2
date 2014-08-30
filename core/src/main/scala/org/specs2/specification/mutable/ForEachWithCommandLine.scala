package org.specs2
package specification.mutable

import execute._
import specification.core.Fragment
import specification.dsl.mutable.ExampleDsl
import specification.create.S2StringContext

/**
 * ForeachWithCommandLine trait, adapted for mutable specifications
 */
trait ForEachWithCommandLine[T] extends specification.ForEachWithCommandLineArguments[T] with ExampleDsl { outer: S2StringContext =>
  override implicit def blockExample(d: String) = new BlockExample1(d)

  class BlockExample1(d: String) extends BlockExample(d) {
    def >>[R : AsResult](f: T => R): Fragment =
      >>(foreachFunctionToExecution(f))

    def in[R : AsResult](f: T => R): Fragment =
      d >> f
  }

}

