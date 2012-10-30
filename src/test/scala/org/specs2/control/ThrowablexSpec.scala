package org.specs2
package control

import specification.Grouped

class ThrowablexSpec extends Specification with Throwablex with Grouped { def is =
                                                                                                                        """
The Throwablex trait provides extensions to regular throwables:
                                                                                                                        """^
  "It provides methods to access nested exceptions"						                                                          ^
    "chainedExceptions returns a list of nested exceptions" 			                                                      ! g1.e1^
    "getFullStackTrace returns a list of all nested stackTraceElements"                                                 ! g1.e2^
    "messageAndCause returns the exception message and its cause if any"                                                ! g1.e3^
                                                                                                                        p^
  "It has location methods"                                                                                             ^
    "to get the name of the file and the line from an exception"                                                        ! g2.e1^
    "to get the class name and line number of an exception"                                                             ! g2.e2^
    "to get the class name, file name and line number of an exception"                                                  ! g2.e3^
    "to get the path of an exception"                                                                                   ! g2.e4^
                                                                                                                        p^
  "It allows to filter stacktraces"                                                                                     ^
    "to filter all the lines matching a given pattern"                                                                  ! g3.e1^
    "to filter all the lines not matching a given pattern"                                                              ! g3.e2^
                                                                                                                        p^
  "It provides utility functions for stacktrace elements"                                                               ^
    "apply returns the nth element"                                                                                     ! g4.e1^
    "headOption returns the first element as an option"                                                                 ! g4.e2^
                                                                                                                        end

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
  "stack" - new g5 with ThrowablexContext {
    e1 := e(3).toString must beMatching(".*apply.*")
    e2 := e.headOption.map(_.toString).toIterable must containMatch("ThrowablexContext")
  }
}