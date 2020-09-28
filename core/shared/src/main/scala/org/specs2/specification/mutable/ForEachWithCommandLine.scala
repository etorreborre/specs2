package org.specs2
package specification.mutable

import execute._
import specification.core.Fragment
import specification.dsl.mutable.NoExampleDsl
import specification.dsl.mutable.ExampleDsl
import specification.create.S2StringContext
import scala.implicits.Not

/**
 * ForEachWithCommandLine trait, adapted for mutable specifications
 */
trait ForEachWithCommandLine[T] extends specification.ForEachWithCommandLineArguments[T] with ExampleDsl { outer: S2StringContext =>

  extension [R : AsResult](d: String)(using not: Not[NoExampleDsl])
    def >>(f: T => R): Fragment =
      addExample(d, foreachFunctionToExecution(f))

    def in(f: T => R): Fragment =
      d >> f

}
