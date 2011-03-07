package org.specs2
package control
import execute._
import specification._

class ThrowablexSpec extends SpecificationWithJUnit with Throwablex { def is =
                                                                                                                        """
The Throwablex trait provides extensions to regular throwables:
                                                                                                                        """^
  "It provides methods to access nested exceptions"						                                                          ^
    "chainedExceptions returns a list of nested exceptions" 			                                                      ! chained.e1^
    "getFullStackTrace returns a list of all nested stackTraceElements"                                                 ! chained.e2^
                                                                                                                        p^
  "It has location methods"                                                                                             ^
    "to get the name of the file and the line from an exception"                                                        ! location.e1^
    "to get the class name and line number of an exception"                                                             ! location.e2^
    "to get the class name, file name and line number of an exception"                                                  ! location.e3^
                                                                                                                        p^
  "It allows to filter stacktraces"                                                                                     ^
    "to filter all the lines matching a given pattern"                                                                  ! filter.e1^
    "to filter all the lines not matching a given pattern"                                                              ! filter.e2^
                                                                                                                        p^
  "It provides utility functions for stacktrace elements"                                                               ^
    "apply returns the nth element"                                                                                     ! stack.e1^
    "headOption returns the first element as an option"                                                                 ! stack.e2^
                                                                                                                        end

  object chained extends ThrowablexContext {
	  def e1 = e.chainedExceptions must_== List(e.getCause)
	  def e2 = e.getFullStackTrace.size must_==
	           e.getStackTrace.size + e.getCause.getStackTrace.size
  }
  object location extends ThrowablexContext {
    def e1 = e.location must_== "ThrowablexContext.scala:6"
    def e2 = e.classLocation must_== "org.specs2.control.ThrowablexContext:6"
    def e3 = e.fullLocation must_== "org.specs2.control.ThrowablexContext (ThrowablexContext.scala:6)"
  }
  object filter extends ThrowablexContext {
    def e1 = e.filter("org.specs2.control").getStackTrace.toList.map(_.toString) must
               containMatch("org.specs2.control")
    def e2 = e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must not
               containMatch("org.specs2.control")
  }
  object stack extends ThrowablexContext {
    def e1 = e(3).toString aka e.getStackTrace().mkString("\n") must beMatching(".*apply.*")
    def e2 = e.headOption.map(_.toString).toIterable must containMatch("ThrowablexContext")
  }
}