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

"""
}
