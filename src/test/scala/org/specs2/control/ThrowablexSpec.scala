package org.specs2
package control
import execute._
import specification._

class ThrowablexSpec extends SpecificationWithJUnit with Throwablex { def is =
                                                                                          """  
  The Throwablex trait provides extensions to regular throwables:                         
                                                                                          """^
"  It provides methods to access nested exceptions"						                            ^
"    chainedExceptions returns a list of nested exceptions" 			                        ! chained().e1^
"    getFullStackTrace returns a list of all nested stackTraceElements"                   ! chained().e2^
                                                                                          p^
"  It has location methods"                                                               ^
"    to get the name of the file and the line from an exception"                          ! c().e1^
"    to get the class name and line number of an exception"                               ! c().e2^
"    to get the class name, file name and line number of an exception"                    ! c().e3^
                                                                                          p^
"  It allows to filter stacktraces"                                                       ^
"    to filter all the lines matching a given pattern"                                    ! c().e4^
"    to filter all the lines not matching a given pattern"                                ! c().e5^
                                                                                          p^
"  It provides utility functions for stacktrace elements"                                 ^
"    apply returns the ith element"                                                       ! c().e6^
"    headOption returns the first element as an option"                                   ! c().e7^
                                                                                          end

  case class c() extends ThrowablexContext {
  def e1 = e.location must_== "ThrowablexContext.scala:6"
    def e2 = e.classLocation must_== "org.specs2.control.ThrowablexContext:6"
    def e3 = e.fullLocation must_== "org.specs2.control.ThrowablexContext (ThrowablexContext.scala:6)"
    def e4 = e.filter("org.specs2.control").getStackTrace.toList.map(_.toString) must 
               containMatch("org.specs2.control")
    def e5 = e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must 
               containMatch("org.specs2.control").not
    def e6 = e(2).toString must beMatching(".*apply.*")
    def e7 = e.headOption.map(_.toString).toIterable must containMatch("ThrowablexContext")
  }
  case class chained() extends ThrowablexContext {
	  def e1 = e.chainedExceptions must_== List(e.getCause)
	  def e2 = e.getFullStackTrace.size must_== 
	           e.getStackTrace.size + e.getCause.getStackTrace.size
  }
}