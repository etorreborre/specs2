package org.specs2

class DependenciesSpec extends SpecificationWithJUnit { def is = 
                                                                                          """
  The following dependencies must be enforced in specs2:
                                                                                          """ ^
                                                                                          """
  o    runner
  o    reporter 
  o    specification  
  o    mock form
  *    matcher  
  *    execute  
  *               reflect            xml 
  *    collection control  io  text  main
                                                                                          """ ^
                                                                                          end

}