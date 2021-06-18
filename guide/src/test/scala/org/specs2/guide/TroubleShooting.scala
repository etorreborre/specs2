package org.specs2
package guide

import org.specs2.execute.{Success, ResultExecution, AsResult}

object Troubleshooting extends UserGuidePage { def is = "Troubleshooting".title ^ s2"""

This section presents some of the common pitfalls you might face when using $specs2 and Scala

### vals vs lazy vals

The common symptom here is a `NullPointerException` for some attributes of your specification. You can refer to this [link](http://engineering.tapad.com/blog/2012/11/here-there-be-dragons-dangers-of-initialization-order-in-scala/) for an explanation of variable initialization in Scala. In the meantime the basic fix is to use a `lazy val` instead of a `val`.

### Lost expectations

You might expect the following specification to fail:${snippet{
class ShouldItFail extends Specification { def is = s2"""
  Should this example fail? $e1
"""
  def e1 = {
    1 must ===(100000) // do you expect this to fail
    10 must ===(10)
  }
}
}}

However, as explained in ${see(Structure)} - Thrown expectations, the first expectation is lost because, by default, no exceptions are thrown in an acceptance specification.
In that case you can either:

 - create 2 examples having one expectation each
 - mix-in the `ThrownExpectations` trait

### No evidence parameter for AsResult

Here is a mysterious `AsResult` message
```
class MysteriousAsResult extends mutable.Spec {
  "Try this" >> {
    1 must ===(1)
    println("this is ok, right?")
  }
}

[error] could not find implicit value for evidence parameter of type org.specs2.execute.AsResult[Unit]
```

What's happening? Each example must return a value of type `T` where `T` has an `AsResult` instance. This is the case if `T` is a `Boolean`, a `Result`, a ScalaCheck `Prop` etc... (see ${see(AsResultTypeclass)}). In the class above the return value is `Unit` which doesn't have an `AsResult` instance.

To fix this you can either:

 - either return `ok` after `println`
 - create your own matcher (see ${see(Matchers)}) and make sure you have a proper expectation for your example
 - add an `AsResult[Unit]` instance like this
<p/>
${snippet{
given AsResult[Unit] with
  def asResult(r: =>Unit) =
    ResultExecution.execute(r)(_ => Success())
}}

### Custom output

You created a custom `Notifier` or another class for reporting and when you use it nothing happens. In that case add the `verbose` argument to the command line and you will get more messages including exceptions and stacktraces about what is going on.

"""
}
