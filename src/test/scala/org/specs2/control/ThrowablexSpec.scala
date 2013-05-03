package org.specs2
package control

import specification._

class ThrowablexSpec extends script.Specification with Throwablex with Grouped { def is = section("unstable") ^ s2"""

The Throwablex trait provides extensions to regular throwables:

 It provides methods to access nested exceptions
   + chainedExceptions returns a list of nested exceptions
   + getFullStackTrace returns a list of all nested stackTraceElements
   + messageAndCause returns the exception message and its cause if any

 It has location methods
   + to get the name of the file and the line from an exception
   + to get the class name and line number of an exception
   + to get the class name, file name and line number of an exception
   + to get the path of an exception

 It allows to filter stacktraces
   + to filter all the lines matching a given pattern
   + to filter all the lines not matching a given pattern

 It provides utility functions for stacktrace elements
   + apply returns the nth element
   + headOption returns the first element as an option
                                                                                                                 """

  "chained" - new g1 with ThrowablexContext {
	  e1 := e.chainedExceptions      === List(e.getCause)
	  e2 := e.getFullStackTrace.size === e.getStackTrace.size + e.getCause.getStackTrace.size
    e3 := e.messageAndCause        === "message. Cause: cause"
  }
  "location" - new g2 with ThrowablexContext {
    e1 := e.location                === "ThrowablexContext.scala:6"
    e2 := e.classLocation           === "org.specs2.control.ThrowablexContext:6"
    e3 := e.fullLocation            === "org.specs2.control.ThrowablexContext (ThrowablexContext.scala:6)"
    e4 := TraceLocation(trace).path === "org/specs2/control/ThrowablexContext.scala"
  }
  "filter" - new g3 with ThrowablexContext {
    e1 := e.filter("org.specs2.control").getStackTrace.toList.map(_.toString) must containMatch("org.specs2.control")
    e2 := e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must not containMatch("org.specs2.control")
  }
  "stack" - new g4 with ThrowablexContext {
    e1 := e(2).toString aka e.getStackTraceString must beMatching(".*org.specs2.control.ThrowablexSpec\\$.*")
    e2 := e.headOption.map(_.toString).toIterable must containMatch("ThrowablexContext")
  }
}