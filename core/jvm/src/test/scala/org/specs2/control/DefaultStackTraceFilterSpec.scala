package org.specs2
package control

import matcher.*
import user.specification.*
import execute.*

class DefaultStackTraceFilterSpec extends Spec with ThrownExpectations { def is =  s2"""

 the default stacktrace filter should
   remove specs2 traces if coming from user code                      $e1
   truncate the stack trace if it is too long                         $e2

   when using SpecificationLike it should
     remove specs2 methods set on a user class (see #415)             $e3

 It should find if the stacktrace results from a specs2 own specification $e4
"""

  def e1 = {
    val stacktrace = (new UserExpectations).failure1.exception.getStackTrace.toIndexedSeq
    DefaultStackTraceFilter(stacktrace).map(_.toString) must not(containMatch(".*specs2.*")) }

  def e2 =
    val huge = new Exception
    huge.setStackTrace(Array.fill(1200)(new StackTraceElement("class", "method", "file", 1)))
    val filtered = DefaultStackTraceFilter(huge)
    filtered.getStackTrace must haveSize(be_<(1000))
    filtered.getStackTrace.toSeq must containMatch("TRUNCATED")

  def e3 =
    (new UserExpectationsLike).failure1 match

      // the right line must be set as the failure
      // location
      case f: Failure =>
        DefaultStackTraceFilter(f.stackTrace).map(_.toString) must containMatch("UserExpectations.scala:17")
      case _ => ok

  // see issue #533
  def e4 =
    AsResult((new UserExpectationsSpec).failure1) match

      // the right line must be set as the failure
      // location
      case f: Failure =>
        DefaultStackTraceFilter(f.stackTrace).map(_.toString) must containMatch("UserExpectations.scala:20")
      case _ => ok


}
