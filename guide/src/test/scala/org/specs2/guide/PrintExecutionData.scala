package org.specs2
package guide

import execute.{ResultExecution, Result, AsResult}
import specification.core.Fragment
import specification.{AroundEach, Around}
import org.specs2.specification.create.{DefaultFragmentFactory}
import time.SimpleTimer

object PrintExecutionData extends UserGuidePage { def is = s2"""

### Print success data

If an example returns returns a `Success` we just print the example name on the console but it can be interesting to also get some information about the data the example was executed with. In order to do that you can use the `updateExpected` method and pass a non-empty string with your message: ${snippet{
  "this is an obvious example" ! {
    val i = 1
    (i must_== 1).toResult.updateExpected("executed with "+i)
  }
}}

Let's use this method now to display the execution time for each example.

### Print execution time

Knowing that an example succeeded is fine but sometimes you want to display more information, like the time spent executing the example for instance, or some other state before and after each example.

This can be done by using the `AroundEach` trait and updating the `Result` of the example execution with whatever you want to display: ${snippet{

trait Timed extends AroundEach {
  def around[T : AsResult](t: =>T): Result = {
    // use `ResultExecution.execute` to catch possible exceptions
    val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))

    // update the result with a piece of text which will be displayed in the console
    result.updateExpected("Execution time: "+timer.time)
  }

  /** mesure the execution time of a piece of code */
  def withTimer[T](t: =>T): (T, SimpleTimer) = {
    val timer = (new SimpleTimer).start
    val result = t
    (result, timer.stop)
  }
}
}}

When you execute a specification mixing the `Timed` trait you should see the timing of each example displayed in the console:

```
[info] TimedExecutionSpecification
[info]
[info] + example 1
[info] Execution time: 94 ms
[info] + example 2
[info] Execution time: 11 ms
```

Note that this is just an example. The same functionality is actually accessible with the [`showtimes` argument]($ConsoleOutput).

### With the example description

More generally, you can both use the example description and the example body to display custom messages. To do this you need to intercept the creation of examples by creating a new `FragmentFactory`: ${snippet{

// a trait to create an Around context using the example description
trait TimedContext {
  def context(exampleDescription: String) = new Timed(exampleDescription)

  case class Timed(exampleDescription: String) extends Around {
    def around[T : AsResult](t: =>T): Result = {
      val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))
      result.updateExpected(s"Execution time for example $$exampleDescription: $${timer.time}")
    }

    def withTimer[T](t: =>T): (T, SimpleTimer) = {
      val timer = (new SimpleTimer).start
      val result = t
      (result, timer.stop)
    }
  }
}

class TimedSpecification extends Specification with TimedContext { def is = s2"""
 Example 1 $ok
 Example 2 $ok
"""

  // create a new DefaultFragmentFactory where the body of the example uses
  // the current example description
  override lazy val fragmentFactory = new DefaultFragmentFactory {
    override def example[T : AsResult](description: String, t: =>T): Fragment =
      super.example(description, context(description)(AsResult(t)))
  }
}
}}

"""
}
