package org.specs2
package guide

object StandardResults extends UserGuidePage { def is = s2"""
The ***specs2*** ${see(QuickStart)} guide introduces matchers to create expressions but you can use other kinds of predefined results to implement your examples.

### Boolean results

This is the simplest kind of result you can define for an expectation but also the least expressive! It can be useful for simple expectations but a failure will give few information on what went wrong: ${snippet{
  new mutable.Specification {
    "this example is ok" >> {
      1 == 1
    }
    "this one is not" >> {
      // fails with 'the value is false'...
      1 == 2
    }
  }
}}

Not only that but in unit specification no exception will be thrown so you need to `&&` and `||` operators to connect your assertions

### Standard results

Some other results will give you an additional piece of meaning:

  * `success`: when your example is ok
  * `failure`: when your example is incorrect
  * `anError`: when an exception occurs
  * `skipped`: when you want to skip the example. You can add a more specific message with `skipped("because it just doesn't work in these conditions")`
  * `pending`: usually means "not implemented yet", but you can add a more specific message with `pending("because I don't have time for this")`

Two additional results are also available to track the progress of features:

  * `done`: a `Success` with the message "DONE"
  * `todo`: a `Pending` with the message "TODO"

### Skipping an example

If you already have some code for your example, adding `skipped` at the end to skip it is not very efficient:

 - there is a possibility that the code throws an exception
 - the code will be executed which will waste resources

What you want in that case in to skip the whole block:${snippet{
  s2" this example *must* be skipped $e1"

  def e1 = skipped {
    // whatever code is in there, it will not be executed and the result will be skipped
    throw new Exception("uh-oh")
    1 === 1
  }
}}

### Setting an example as Pending

Similarly you can mark the example as `Pending`:${snippet{
  s2" this example is pending for now$e1"

  def e1 = pending {
    // whatever code is in there, it will not be executed and the result will be pending
    throw new Exception("uh-oh")
    1 === 1
  }
}}

### Standard `MatchResults`

When combining matchers you might be expected to return a `MatchResult[_]`. There are predefined values for those too:

 * `ok` or `ok(message)` for a successful `MatchResult`
 * `ko` or `ko(message)` for a failed `MatchResult`

"""
}
