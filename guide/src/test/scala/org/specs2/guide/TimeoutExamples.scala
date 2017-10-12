package org.specs2
package guide

import concurrent.ExecutionEnv

object TimeoutExamples extends UserGuidePage { def is = s2"""

### One example

Some examples take too long to run and it is desirable to interrupt them if they do. This is the purpose of the `org.specs2.specification.AroundTimeout` trait: ${snippet{
import org.specs2.specification.AroundTimeout
import scala.concurrent.duration._

class MySpec(implicit ee: ExecutionEnv) extends Specification with AroundTimeout { def is = s2"""

  this should not take too long ${upTo(1.second)(e1)}

"""

  def e1 = { 1 + 1 === 2 }
}
}}

Note that you need to extend the `org.specs2.specification.ExecutionEnvironment` trait to get an implicit `ExecutionEnv` in order to use this functionality (read more on the $ExecutionEnvironments).

### The whole specification

You can also declare a timeout for all the examples of a given specification with the `org.specs2.specification.ExamplesTimeout` trait:${snippet{
import org.specs2.specification.ExamplesTimeout

class MySpec(implicit ee: ExecutionEnv) extends Specification with ExamplesTimeout { def is = s2"""

  this should not take too long $e1
  this one too                  $e2

"""

  def e1 = { 1 + 1 === 2 }
  def e2 = { 2 + 2 === 4 }
}
}}

With the `ExamplesTimeout` trait you can control the duration of the time out by passing the `timeout` argument on the command-line (in milliseconds).

"""

}
