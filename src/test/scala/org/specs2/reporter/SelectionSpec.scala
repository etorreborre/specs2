package org.specs2
package reporter

class SelectionSpec extends SpecificationWithJUnit { def is =
                                                                                          """
  Before executing and reporting a specification, the fragments must be selected and 
  sorted:
  
  * they must be selected to keep only the relevant ones
  * they must be sorted to respect their execution dependencies
    * steps must be executed before examples as specified
    * tagged examples with dependencies must respect the corresponding ordering
                                                                                          """^
  "If a specification contains steps they must always be executed before examples"        ! e1
                                                                                          end
                                                                                          
  def e1 = pending                                                                                          

}