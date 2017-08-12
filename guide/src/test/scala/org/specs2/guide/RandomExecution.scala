package org.specs2
package guide

import specification.process.RandomSequentialExecution

object RandomExecution extends UserGuidePage { def is = s2"""
 The ${"execution" ~/ Execution} page describes the most frequent modes of execution:

  - fully concurrent
  - concurrent with steps
  - fully sequential

Executing the specification concurrently in particular can be a good way to test the stability of the system in presence of random commands. However it is sometimes not possible to overlap examples executions because that could break the external state.

### Random sequential execution

In this case you can use another mode of execution. By mixing-in the `org.specs2.specification.process.RandomSequentialExecution` trait some execution constraints will be randomly added to your examples so that they will force your examples to execute in a random order, one after the other. This randomization is only being done for examples in between steps so if you have steps inside the specification guaranteeing some kind of checkpoints during the execution, they will be preserved.

Let's see this on an example:${snippet{
class RandomSequentialSpec extends Specification with RandomSequentialExecution { def is = s2"""
 example1 $e1
 example2 $e2
 example3 $e3
 ${step("here".pp)}
 example4 $e4
 example5 $e5
 example6 $e6
"""

  def e1 = { "e1".pp; ok }
  def e2 = { "e2".pp; ok }
  def e3 = { "e3".pp; ok }

  def e4 = { "e4".pp; ok }
  def e5 = { "e5".pp; ok }
  def e6 = { "e6".pp; ok }
}
}}

With such a specification you might see in the console:
```
[info] e3
[info] e2
[info] e1
[info] here
[info] e5
[info] e6
[info] e4
```

### Using ScalaCheck

The `RandomSequentialExecution` trait is actually a very naive way to test random commands on a system. A much better approach is to use ScalaCheck and its notion of [stateful property-based testing](http://www.scalacheck.org/files/scaladays2014/index.html).

"""
}
