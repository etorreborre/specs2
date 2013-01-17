package org.specs2
package control

import specification.Grouped
import matcher.ThrownExpectations

class DefaultStackTraceFilterSpec extends Specification with Grouped with ThrownExpectations { def is =

  "the default stacktrace filter should"         ^
    "remove specs2 traces"                       ! g1.e1^
    "truncate the stack trace if it is too long" ! g1.e2^
                                                 end

  "filter" - new g1 {
    e1 := { DefaultStackTraceFilter.apply(new Exception).getStackTrace.map(_.toString).toSeq must not containMatch(".*specs2.*") }
    e2 := {
      val huge = new Exception
      huge.setStackTrace(Array.fill(1200)(new StackTraceElement("class", "method", "file", 1)))
      val filtered = DefaultStackTraceFilter.apply(huge)
      filtered.getStackTrace.size must be_<(1000)
      filtered.getStackTrace.toSeq must containMatch("TRUNCATED")
    }
  }



}
