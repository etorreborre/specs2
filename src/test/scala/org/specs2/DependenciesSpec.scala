package org.specs2

class DependenciesSpec extends Specification { def is = 
                                                                                          """
  The following dependencies must be enforced in specs2:
                                                                                          """ ^
                                                                                          """
  +    runner
  +    reporter 
  +    specification  
  +    mock form
  +    matcher  
  +    execute  
  +               reflect  xml time
  +    collection control  io  text  main data
                                                                                          """ ^
                                                                                          end

}