package org.specs2
package control

import Throwablex._

class IncludeExcludeStackTraceFilterSpec extends IncludeExcludeStackTraceFilterExamples { def is = s2"""

Patterns
========
  A stacktrace can be filtered
    by declaring 'exclude' patterns
      leaving the elements not matching the patterns           $exclude1
      filtering out the elements matching the patterns         $exclude2

    by declaring 'include' patterns
      leaving the elements matching the patterns               $include1
      filtering out the elements not matching the patterns     $include2

Creation
========

  A IncludedExcludedStackTraceFilter can be created from a string
    the default pattern is i1,i2/e1,e2 where i are include tags and e are exclude tags $includeExclude1

  From an existing IncludedExcludedStackTraceFilter
    we can add more include patterns, using the includeAlso method $includeExclude2
    we can add more exclude patterns, using the excludeAlso method $includeExclude3

Filtering
=========

  A StackTraceFilter, when filtering an exception should
    retain the exception cause $filter1
    retain the exception type  $filter2
"""
}

trait IncludeExcludeStackTraceFilterExamples extends IncludeExcludeStackTraceFilterImplementation {

  def exclude1 = filter(stacktrace("t1", "a", "com.t1.other"))      (excludeTrace("t1", "t2")) must not(containMatch("t1"))
  def exclude2 = filter(stacktrace("t1", "t3", "a", "com.t1.other"))(excludeTrace("t1", "t2")) must containMatch("t3")

  def include1 = filter(stacktrace("t1", "a", "com.t1.other"))      (includeTrace("t1", "t2")) must containMatch("t1")
  def include2 = filter(stacktrace("t1", "t3", "a", "com.t1.other"))(includeTrace("t1", "t2")) must not(containMatch("t3"))

  def includeExclude1 = IncludeExcludeStackTraceFilter.fromString("i1,i2/e1,e2") must_==
    IncludeExcludeStackTraceFilter(Seq("i1", "i2"), Seq("e1", "e2"))

  val defaultFilter = DefaultStackTraceFilter

  def includeExclude2 = filter(stacktrace("org.specs2", "t1"))(defaultFilter.includeAlso("t1", "t2")) must not(containMatch("specs2"))
  def includeExclude3 = filter(stacktrace("org.specs2", "t1"))(defaultFilter.excludeAlso("t1"))       must not(containMatch("t1"))


  def filter1 = {
    val cause = new Exception("bang")
    DefaultStackTraceFilter.apply(new Exception("boom", cause)).getCause must_== cause
  }

  def filter2 = DefaultStackTraceFilter.apply(new IllegalArgumentException("ohnoes")).getClass.getName ===
    "java.lang.IllegalArgumentException"

}

trait IncludeExcludeStackTraceFilterImplementation extends Specification {
  def stacktrace(st: String*) =  st.map(stackTraceElement(_))

  /** filter a stacktrace */
  def filter(stacktrace: Seq[StackTraceElement])(f: IncludeExcludeStackTraceFilter) =
    f(stacktrace).map(_.toString)
}
