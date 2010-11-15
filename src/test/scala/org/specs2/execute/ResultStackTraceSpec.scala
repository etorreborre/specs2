package org.specs2
package execute

class ResultStackTraceSpec extends SpecificationWithJUnit {  def is =
  
" A StackTrace for a Result must sanitize the stacktrace"                                 ^
" to present only a relevant stacktrace element for the user"                             ^
"   if it is a a user specification, the 'org.specs2' lines are filtered"                 ! e1^
"   if it is a a specs2 specification, the 'org.specs2' lines are not filtered"           ! e2^
"   if it is a a user specification, only the lines containing 'Result' are filtered"     ! e3^
                                                                                          end

  def e1 = locationMustBe(
		  List(("org.specs2.runner", "Runner.scala", 8), 
		       ("com.mycompany",     "MySpec.scala", 3)), "MySpec.scala:3")
		  
  def e2 = locationMustBe(
		  List(("org.specs2.specs2Spec", "specs2Spec.scala", 3),
		 	     ("org.specs2.runner",     "Runner.scala",     8)), "specs2Spec.scala:3")

  def e3 = locationMustBe(
		  List(("org.specs2.Result",     "Runner.scala",     8), 
		       ("org.specs2.specs2Spec", "specs2Spec.scala", 3)), "specs2Spec.scala:3")

  def locationMustBe(messages: Seq[(String, String, Int)], expected: String) =
	  stackTraceResult(messages:_*).location must_== expected

  import control._

  case class stackTraceResult(msg: (String, String, Int)*) extends ResultStackTrace {
	  def stackTrace = {
	    msg.foldLeft(Nil: List[StackTraceElement]) { (res, cur) =>
	      res :+ Throwablex.stackTraceElement(cur._1, fileName = cur._2, lineNumber = cur._3)
	    }
	  }
  } 
}