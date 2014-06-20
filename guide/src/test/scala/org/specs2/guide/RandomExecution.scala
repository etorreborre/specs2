package org.specs2
package guide

object RandomExecution extends UserGuidePage { def is = s2"""
 The ${"execution" ~ Execution} page describes the most frequent modes of execution:

  - fully concurrent
  - concurrent with steps
  - fully sequential

Another useful mode of execution is when examples are executed sequentially but where is execution order is random. This can be achieved by mixing-in the `org.specs2.specification.process.RandomExecution` trait. This trait is going to randomly add execution constraints between your examples so that they will be forced to execute in a random order.

"""
}
