package org.specs2
package control

import matcher.{ValueChecks, ThrownExpectations}
import user.specification.UserExpectations

class DefaultStackTraceFilterSpec extends Spec with ThrownExpectations { def is =  s2"""

 the default stacktrace filter should
   remove specs2 traces if coming from user code                      $e1
   truncate the stack trace if it is too long                         $e2
                                                                      """

  def e1 = { DefaultStackTraceFilter((new UserExpectations).failure1.exception.getStackTrace).map(_.toString).toSeq must not(containMatch(".*specs2.*")) }

  def e2 = {
    val huge = new Exception
    huge.setStackTrace(Array.fill(1200)(new StackTraceElement("class", "method", "file", 1)))
    val filtered = DefaultStackTraceFilter(huge)
    filtered.getStackTrace must haveSize(be_<(1000))
    filtered.getStackTrace.toSeq must containMatch("TRUNCATED")
  }

}
