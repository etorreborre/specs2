package org.specs2
package control

import Throwablex._
import specification._

class IncludeExcludeStackTraceFilterSpec extends IncludeExcludeStackTraceFilterExamples { def is = s2"""

  A stacktrace can be filtered
    by declaring 'exclude' patterns
      leaving the elements not matching the patterns                                               ${g1.e1}
      filtering out the elements matching the patterns                                             ${g1.e2}

    by declaring 'include' patterns
      leaving the elements matching the patterns                                                   ${g1.e3}
      filtering out the elements not matching the patterns                                         ${g1.e4}

  A IncludedExcludedStackTraceFilter can be created from a string
    the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags             ${g2.e1}

  From an existing IncludedExcludedStackTraceFilter
    we can add more include patterns, using the includeAlso method                                 ${g3.e1}
    we can add more exclude patterns, using the excludeAlso method                                 ${g3.e2}

  A StackTraceFilter, when filtering an exception should
    retain the exception cause                                                                     ${g4.e1}
    retain the exception type                                                                      ${g4.e2}
                                                                                                   """
}

trait IncludeExcludeStackTraceFilterExamples extends IncludeExcludeStackTraceFilterImplementation with Grouped {

  "patterns" - new g1 {
    e1 := filter(stacktrace("t1", "a", "com.t1.other"))      (excludeTrace("t1", "t2")) must not containMatch("t1")
    e2 := filter(stacktrace("t1", "t3", "a", "com.t1.other"))(excludeTrace("t1", "t2")) must containMatch("t3")
    e3 := filter(stacktrace("t1", "a", "com.t1.other"))      (includeTrace("t1", "t2")) must containMatch("t1")
    e4 := filter(stacktrace("t1", "t3", "a", "com.t1.other"))(includeTrace("t1", "t2")) must not containMatch("t3")
  }

  "from string" - new g2 {
    e1 := IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_==
          IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))
  }

  "from an existing filter" - new g3 {
    val defaultFilter = DefaultStackTraceFilter

    e1 := filter(stacktrace("org.specs2", "t1"))(defaultFilter.includeAlso("t1", "t2")) must not containMatch("specs2")
    e2 := filter(stacktrace("org.specs2", "t1"))(defaultFilter.excludeAlso("t1"))       must not containMatch("t1")
  }

  "exceptions" - new g4 {
    e1 := {
      val cause = new Exception("bang")
      DefaultStackTraceFilter.apply(new Exception("boom", cause)).getCause must_== cause
    }

    e2 := DefaultStackTraceFilter.apply(new IllegalArgumentException("ohnoes")).getClass.getName ===
        "java.lang.IllegalArgumentException"
  }
}

trait IncludeExcludeStackTraceFilterImplementation extends Specification {
  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  /** filter a stacktrace */
  def filter(stacktrace: Seq[StackTraceElement])(f: IncludeExcludeStackTraceFilter) =
    f(stacktrace).map(_.toString)
}
