package org.specs2
package reporter

class ExecutionStrategySpec extends SpecificationWithJUnit { def is = 
                                                                                          """
  By default a Specification is supposed to be executed as concurrently as possible.
  However we must ensure that steps that are inserted before and after all examples
  are executed in order. Otherwise, things like database setups will not be 
  done before the examples.
                                                                                          """
}