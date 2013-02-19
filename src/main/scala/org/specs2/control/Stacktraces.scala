package org.specs2
package control

/**
 * This trait contains helper functions for stacktraces
 */
private[specs2]
trait Stacktraces {
  /**
   * This method is used to determine for example if the JUnit runner is executed from Maven or within Eclipse.
   * In the first the test case names don't need to have the hashcode example.
   *
   * @return true if the this current piece of code contains name in its stacktrace.
   */
  def isExecutedFrom(name: String) = new Exception().getStackTrace().exists(_.toString contains name)

}

private[specs2]
object Stacktraces extends Stacktraces