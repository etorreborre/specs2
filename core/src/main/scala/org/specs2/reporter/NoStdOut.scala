package org.specs2
package reporter

import execute.{Result, AsResult}
import org.specs2.specification._
import java.io.{FileDescriptor, FileOutputStream, OutputStream, PrintStream}

/**
 * This trait allows to remove any console display during the execution of an example
 *
 * Of course it needs to be understood that the output might be completely messed up when
 * executing specifications and examples concurrently
 *
 */
trait NoStdOut extends Around {
  def around[T : AsResult](t: =>T): Result = {
    try {
      // both System.out and Console.out must be swapped because Console keeps
      // a variable containing System.out
      System.setOut(noOut)
      Console.setOut(noOut)
      AsResult(t)
    } finally {
      System.setOut(stdOut)
      Console.setOut(stdOut)
    }
  }

}

object NoStdOut extends NoStdOut

object noOut extends PrintStream(NullOutputStream) {
  override def print(s: String) = ()
}

object stdOut extends PrintStream(new FileOutputStream(FileDescriptor.out))

object NullOutputStream extends OutputStream {
  def write(b: Int) = ()
}

/**
 * This trait allows to remove any console display during the execution of the examples
 * of a Specification
 */
trait NoStdOutAroundExample extends AroundExample {
  def around[T : AsResult](t: =>T): Result = NoStdOut(t)
}
