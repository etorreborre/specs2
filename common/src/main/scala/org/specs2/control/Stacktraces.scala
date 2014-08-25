package org.specs2
package control

/**
 * Utility methods to determine the origin of the execution of the current code
 */
trait Stacktraces {
  /**
   * This method is used to determine for example if the JUnit runner is executed from Maven or within Eclipse.
   * In the first the test case names don't need to have the hashcode example.
   *
   * @return true if this current piece of code contains name in its stacktrace.
   */
  def isExecutedFrom(name: String): Boolean =
    isExecutedFrom(name, (new Exception).getStackTrace.toSeq)

  /* @return true if the stacktrace contains 'name' */
  def isExecutedFrom(name: String, st: Seq[StackTraceElement]): Boolean =
    st.exists(_.toString contains name)

  /** @return true if there is a stacktrace element satisfying the predicate */
  def isFromClass(classNamePredicate: String => Boolean): Boolean =
    isFromClass(classNamePredicate, (new Exception).getStackTrace.toSeq)

  /** @return true if there is a stacktrace element satisfying the predicate */
  def isFromClass(classNamePredicate: String => Boolean, st: Seq[StackTraceElement]): Boolean =
    st.exists(t => classNamePredicate(t.getClassName))
}

object Stacktraces extends Stacktraces
