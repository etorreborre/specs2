package org.specs2
package control

import Throwablex._
import specification._

class IncludeExcludeStackTraceFilterSpec extends IncludeExcludeStackTraceFilterExamples { def is = s2"""

  A stacktrace can be filtered
    by declaring 'exclude' patterns
      + leaving the elements not matching the patterns
      + filtering out the elements matching the patterns
    by declaring 'include' patterns
      + leaving the elements matching the patterns
      + filtering out the elements not matching the patterns

  A IncludedExcludedStackTraceFilter can be created from a string
    + the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags

  From an existing IncludedExcludedStackTraceFilter
    + we can add more include patterns, using the includeAlso method
    + we can add more exclude patterns, using the excludeAlso method

  A StackTraceFilter, when filtering an exception should
    + retain the exception cause
    + retain the exception type
                                                                                                   """
}

trait IncludeExcludeStackTraceFilterExamples extends IncludeExcludeStackTraceFilterImplementation with Grouped {

  "patterns" - new group {
    eg := filter(stacktrace("t1", "a", "com.t1.other"))      (excludeTrace("t1", "t2")) must not containMatch("t1")
    eg := filter(stacktrace("t1", "t3", "a", "com.t1.other"))(excludeTrace("t1", "t2")) must containMatch("t3")
    eg := filter(stacktrace("t1", "a", "com.t1.other"))      (includeTrace("t1", "t2")) must containMatch("t1")
    eg := filter(stacktrace("t1", "t3", "a", "com.t1.other"))(includeTrace("t1", "t2")) must not containMatch("t3")
  }

  "from string" - new group {
    eg := IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_==
          IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))
  }

  "from an existing filter" - new group {
    val defaultFilter = DefaultStackTraceFilter

    eg := filter(stacktrace("org.specs2", "t1"))(defaultFilter.includeAlso("t1", "t2")) must not containMatch("specs2")
    eg := filter(stacktrace("org.specs2", "t1"))(defaultFilter.excludeAlso("t1"))       must not containMatch("t1")
  }

  "exceptions" - new group {
    eg := {
      val cause = new Exception("bang")
      DefaultStackTraceFilter.apply(new Exception("boom", cause)).getCause must_== cause
    }

    eg := DefaultStackTraceFilter.apply(new IllegalArgumentException("ohnoes")).getClass.getName ===
        "java.lang.IllegalArgumentException"
  }
}

trait IncludeExcludeStackTraceFilterImplementation extends script.Specification {
  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  /** filter a stacktrace */
  def filter(stacktrace: Seq[StackTraceElement])(f: IncludeExcludeStackTraceFilter) =
    f(stacktrace).map(_.toString)
}
