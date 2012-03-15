package org.specs2
package control
import Throwablex._

class IncludeExcludeStackTraceFilterSpec extends Specification { def is =

  "A stacktrace can be filtered"                                                                                        ^
    "by declaring 'exclude' patterns"                                                                                   ^
      "leaving the elements not matching the patterns"                                                                  ! e1^
      "filtering out the elements matching the patterns"                                                                ! e2^
                                                                                                                        p^
    "by declaring 'include' patterns"                                                                                   ^
      "leaving the elements matching the patterns"                                                                      ! e3^
      "filtering out the elements not matching the patterns"                                                            ! e4^
                                                                                                                        endp^
  "A IncludedExcludedStackTraceFilter can be created from a string"                                                     ^
    "the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags"                                ! e5^
                                                                                                                        p^
  "From an existing IncludedExcludedStackTraceFilter"                                                                   ^
    "we can add more include patterns, using the includeAlso method"                                                    ! e6^
    "we can add more exclude patterns, using the excludeAlso method"                                                    ! e7^
                                                                                                                        p^
  "A StackTraceFilter, when filtering an exception should"                                                              ^
    "retain the exception cause"                                                                                        ! e8^
    "retain the exception type"                                                                                         ! e9^
                                                                                                                        end

  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  def e1 = excludeTrace("t1", "t2").apply(stacktrace("t1", "a", "com.t1.other")).map(_.toString) must not containMatch("t1")
  def e2 = excludeTrace("t1", "t2").apply(stacktrace("t1", "t3", "a", "com.t1.other")).map(_.toString) must containMatch("t3")

  def e3 = includeTrace("t1", "t2").apply(stacktrace("t1", "a", "com.t1.other")).map(_.toString) must containMatch("t1")
  def e4 = includeTrace("t1", "t2").apply(stacktrace("t1", "t3", "a", "com.t1.other")).map(_.toString) must not containMatch("t3")

  def e5 = IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_== IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))

  def e6 = DefaultStackTraceFilter.includeAlso("t1", "t2").apply(stacktrace("org.specs2", "t1")).map(_.toString) must not containMatch("specs2")
  def e7 = DefaultStackTraceFilter.excludeAlso("t1").apply(stacktrace("org.specs2", "t1")).map(_.toString) must not containMatch("t1")

  def e8 = {
    val cause = new Exception("bang")
    val e = new Exception("boom", cause)
    DefaultStackTraceFilter.apply(e).getCause must_== cause
  }

  def e9 = DefaultStackTraceFilter.apply(new IllegalArgumentException("ohnoes")).getClass.getName ===
           "java.lang.IllegalArgumentException"

}