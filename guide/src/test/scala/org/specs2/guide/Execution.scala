package org.specs2
package guide

object Execution extends UserGuidePage { def is = s2"""

## Execution

### Parallel by default

***specs2*** examples are executed concurrently by default:

 - this makes the execution faster

 - it encourages to write independent examples when the result of a given example should not be influenced by others

Then starting from this default you can progressively add constraints to get more control over the execution.

### Steps

A `Step` is an action which can be executed anywhere in a specification. When you declare a `Step` like this:${snippet{
class StepSpec extends Specification { def is = s2"""
  this is example 1 $ok
  this is example 2 $ok
  ${step("stop here for a second")}

  this is example 3 $ok
  this is example 4 $ok
"""}
}}

Then the specification will:

  - execute examples 1 and 2 in parallel
  - execute the step
  - execute examples 3 and 4 in parallel

There is no "result" for a step but if it throws an Exception an `Error` will be reported. This will not however stop the execution of the rest of the specification.

### Stop the execution

You can however control if the rest of the specification must be executed by adding some constraints on the step. For example:${snippet{
  class StepWithStopOnErrorSpec extends Specification { def is = s2"""
  this is example 1 $ok
  this is example 2 $ok
  ${step { sys.error("sorry!"); "stop here for a second" }.stopOnError}

  this is example 3 $ok
  this is example 4 $ok
"""}
}}

When this specification is executed examples 3 and 4 will be skipped because the step returns an `Error`. You are not limited to errors and the API gives you:

 - `stopOnFail` stop if there is a failure in the previous examples or in the step
 - `stopOnSkipped` stop if there is a skipped result in the previous examples or in the step
 - `stopWhen(Result => Boolean)` stop if there the `and`-ed result of the previous examples and the step verify a condition

----------


This section summarizes the execution algorithm of a specification based on its fragments:

 1. all the fragments are divided into groups delimited by `Steps`
 2. if the `sequential` argument is present, each fragment goes to its own group
 3. groups are executed sequentially and all the fragments of a given group are executed concurrently
 4. if the `isolated` argument is present, each example is executed in its own version of the Specification
 5. if the `isolated` argument is present, all the `Steps` preceding an example are executed before that example
 6. if the Specification inherits from the `AllExpectations` trait, then it is executed as an `isolated` Specification unless it is already set as `sequential`
 7. if the `stopOnFail` argument is present, all the examples in the next group of fragments will be skipped if there is a failure in one of the previous groups
 8. if the `stopOnSkip` argument is present, all the examples in the next group of fragments will be skipped if there is a skipped in one of the previous groups
 9. if there is a `Step` created with `Step.stopOnFail` or `Step.stopOnFail(when = true)`, all the examples in the next group of fragments will be skipped if there is a failure in the group before the `Step`
  """

}
