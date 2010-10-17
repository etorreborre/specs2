package org.specs2
package execute

class ResultSpec extends SpecificationWithJUnit {
  def is = 
                                                                                          """
  Results are the outcome of some execution. There are several kinds of Results, all
	having a message describing them more precisely:
	  * Success: everything is ok
	  * Failure: an expectation is not met
	  * Error: something completely unexpected happened
	  * Skipped: the user decided to skip the execution for some reason
	  * Pending: the user decided that the execution was not yet implemented
                                                                                          """                                                                                       ^
"  Results can be combined"                                                               ^
"    success1 and success2 == Success(s1 and s2)"                                         ! e1^
"    success1 and success1 == Success(s1)"                                                ! e2^
"    success1 and failure1 == failure1"                                                   ! e3^
                                                                                          end
  val success1 = Success("success1")                                                                                          
  val success2 = Success("success2")                                                                                          
  val failure1 = Failure("failure1")                                                                                          

  def e1 = (success1 and success2) must_== Success("success1 and success2")                                                                                          
  def e2 = (success1 and success1) must_== Success("success1")
  def e3 = ((success1 and failure1) must_== failure1) and ((failure1 and success1) must_== failure1)                                
}    