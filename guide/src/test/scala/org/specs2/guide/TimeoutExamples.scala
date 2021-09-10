package org.specs2
package guide

import concurrent.ExecutionEnv
import scala.concurrent.duration.*

object TimeoutExamples extends UserGuidePage {
  def is = s2"""

### Global timeout

Some examples take too long to run and it is desirable to interrupt them if they do. You can specify a global timeout by passing a value in milliseconds
on the command line:
```
sbt> testOnly -- timeout 500
```

### Specification timeout

It is also possible to specify a specification timeout overriding the global timeout by specifying the timeout argument: ${snippet{
class MySpecification extends Specification:
  def is = args.execute(timeout = 10.seconds) ^ s2"""
  this example should not take too long $e1
  this one too $e2
  """

  def e1 = { 1 + 1 === 2 }
  def e2 = { 2 + 2 === 4 }
}}
"""

}
