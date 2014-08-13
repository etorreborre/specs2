package org.specs2
package control

import specification._

class ThrowablexSpec extends script.Specification with Throwablex with Grouped { def is = section("unstable") ^ s2"""

The Throwablex trait provides extensions to regular throwables:

 # It provides methods to access nested exceptions
   + chainedExceptions returns a list of nested exceptions
   + getFullStackTrace returns a list of all nested stackTraceElements
   + messageAndCause returns the exception message and its cause if any

 # It has location methods
   + to get the name of the file and the line from an exception
   + to get the class name and line number of an exception
   + to get the class name, file name and line number of an exception
   + to get the path of an exception

 # It allows to filter stacktraces
   + to filter all the lines matching a given pattern
   + to filter all the lines not matching a given pattern

 # It provides utility functions for stacktrace elements
   + apply returns the nth element
   + headOption returns the first element as an option
                                                                                                                 """

  "chained" - new group with ThrowablexContext {
    eg := e.chainedExceptions      === List(e.getCause)
    eg := e.getFullStackTrace.size === e.getStackTrace.size + e.getCause.getStackTrace.size
    eg := e.messageAndCause        === "message. Cause: cause"
  }
  "location" - new group with ThrowablexContext {
    eg := e.location                === "ThrowablexContext.scala:6"
    eg := e.classLocation           === "org.specs2.control.ThrowablexContext:6"
    eg := e.fullLocation            === "org.specs2.control.ThrowablexContext (ThrowablexContext.scala:6)"
    eg := TraceLocation(trace).path === "org/specs2/control/ThrowablexContext.scala"
  }
  "filter" - new group with ThrowablexContext {
    eg := e.filter("org.specs2.control").getStackTrace.toList.map(_.toString) must containMatch("org.specs2.control")
    eg := e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must not containMatch("org.specs2.control")
  }
  "stack" - new group with ThrowablexContext {
    eg := e(2).toString aka e.getStackTrace.mkString("\n") must beMatching(".*org.specs2.control.ThrowablexSpec\\$.*")
    eg := e.headOption.map(_.toString).toIterable must containMatch("ThrowablexContext")
  }
}