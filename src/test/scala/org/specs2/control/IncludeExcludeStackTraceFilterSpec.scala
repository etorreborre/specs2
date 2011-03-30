package org.specs2
package control
import Throwablex._

class IncludeExcludeStackTraceFilterSpec extends SpecificationWithJUnit { def is =

  "A stacktrace can be filtered"                                                                                        ^
    "by declaring 'exclude' patterns"                                                                                   ^
      "leaving the elements not matching the patterns"                                                                  ! e1^
      "filtering out the elements matching the patterns"                                                                ! e2^
                                                                                                                        p^
    "by declaring 'include' patterns"                                                                                   ^
      "leaving the elements matching the patterns"                                                                      ! e3^
      "filtering out the elements not matching the patterns"                                                            ! e4^
                                                                                                                        p^
  "A IncludedExcludedStackTraceFilter can be created from a string"                                                     ^
    "the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags"                                ! e5^
                                                                                                                         end

  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  def e1 = excludeTrace("t1", "t2").apply(stacktrace("t1", "a", "com.t1.other")).map(_.toString) must not containMatch("t1")
  def e2 = excludeTrace("t1", "t2").apply(stacktrace("t1", "t3", "a", "com.t1.other")).map(_.toString) must containMatch("t3")

  def e3 = includeTrace("t1", "t2").apply(stacktrace("t1", "a", "com.t1.other")).map(_.toString) must containMatch("t1")
  def e4 = includeTrace("t1", "t2").apply(stacktrace("t1", "t3", "a", "com.t1.other")).map(_.toString) must not containMatch("t3")

  def e5 = IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_== IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))

}