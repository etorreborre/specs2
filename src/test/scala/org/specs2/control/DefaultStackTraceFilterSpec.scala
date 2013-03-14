package org.specs2
package control

import specification.Grouped
import matcher.ThrownExpectations
import execute.Function0Result._
import execute.{AsResult, Function0Result}

class DefaultStackTraceFilterSpec extends Specification with ThrownExpectations { def is =  s2"""

 the default stacktrace filter should
   remove specs2 traces                       $e1
   truncate the stack trace if it is too long $e2

                                                                                              """

  val e1 = { DefaultStackTraceFilter.apply(new Exception).getStackTrace.map(_.toString).toSeq must not containMatch(".*specs2.*") }

  val e2 = {
    val huge = new Exception
    huge.setStackTrace(Array.fill(1200)(new StackTraceElement("class", "method", "file", 1)))
    val filtered = DefaultStackTraceFilter.apply(huge)
    filtered.getStackTrace.size must be_<(1000)
    filtered.getStackTrace.toSeq must containMatch("TRUNCATED")
  }

}
