package org.specs2
package specification.mutable

import execute._
import specification.core.Fragment
import specification.dsl.mutable.ExampleDsl
import specification.create.S2StringContext

/**
 * ForEachWithCommandLine trait, adapted for mutable specifications
 */
trait ForEachWithCommandLine[T] extends specification.ForEachWithCommandLineArguments[T] with ExampleDsl { outer: S2StringContext =>
  override implicit def blockExample1(d: String): BlockExample =
    new BlockExample(d)

  class BlockExample(d: String) extends BlockExample1(d) {
    def >>[R : AsResult](f: T => R): Fragment =
      >>(foreachFunctionToExecution(f))

    def in[R : AsResult](f: T => R): Fragment =
      d >> f
  }

}
