package org.specs2
package reporter
import specification._
import io._

class SelectionSpec extends SpecificationWithJUnit { def is =
                                                                                          """
  Before executing and reporting a specification, the fragments must be selected and 
  sorted:
  
  * they must be selected to keep only the relevant ones
  * they must be sorted to respect their execution dependencies
    * steps must be executed before examples as specified
    * tagged examples with dependencies must respect the corresponding ordering
                                                                                          """^
  "If a specification contains steps they must always be executed before examples"        ! steps().e1
                                                                                          end
  
  case class steps()  {
    val reporter = new ConsoleReporter with MockOutput
    val ex1 = "e1" ! success
    val first = action("first")
    def e1 = {
      report(first ^ ex1) must containInOrder("first", "+ e1")
    }           
    def action(message: String) = Action(reporter.println(message)) 
    def report(f: Fragments) = reporter.report(new Specification { def is = f }).messages                                                                                        
  }

}