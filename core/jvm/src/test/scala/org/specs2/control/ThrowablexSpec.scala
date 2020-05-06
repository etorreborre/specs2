package org.specs2
package control

import ThrowableExamples._

class ThrowablexSpec extends Specification with Throwablex { def is = s2"""

The Throwablex trait provides extensions to regular throwables:

It provides methods to access chained exceptions
 chainedExceptions returns a list of chained exceptions $chained1
 getFullStackTrace returns a list of all chained stackTraceElements $chained2
 messageAndCause returns the exception message and its cause if any $chained3

It has location methods
 to get the name of the file and the line from an exception $location1
 to get the class name and line number of an exception $location2
 to get the class name, file name and line number of an exception $location3
 to get the path of an exception $location4

It allows to filter stacktraces
 to filter all the lines matching a given pattern $filter1
 to filter all the lines not matching a given pattern $filter2

It provides utility functions for stacktrace elements
 apply returns the nth element $stack1
 headOption returns the first element as an option $stack2

"""

  def chained1 = e.chainedExceptions      === List(e.getCause)
  def chained2 = e.getFullStackTrace.size === e.getStackTrace.length + e.getCause.getStackTrace.length
  def chained3 = e.messageAndCause        === "message. Cause: cause"

  def location1 = e.location                === "ThrowableExamples.scala:8"
  def location2 = e.classLocation           === "org.specs2.control.ThrowableExamples:8"
  def location3 = e.fullLocation            === "org.specs2.control.ThrowableExamples (ThrowableExamples.scala:8)"
  def location4 = TraceLocation(trace).path === "org/specs2/control/ThrowableExamples.scala"

  def filter1 = e.filter("org.specs2.specification.core").getStackTrace.toList.map(_.toString) must containMatch("org.specs2.specification.core")
  def filter2 = e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must not (containMatch("org.specs2.control"))

  def stack1 = e(0).toString must beMatching(".*org.specs2.control.ThrowableExamples\\$.e.*")
  def stack2 = e.headOption.map(_.toString).toIterable must containMatch("ThrowableExamples")
}
