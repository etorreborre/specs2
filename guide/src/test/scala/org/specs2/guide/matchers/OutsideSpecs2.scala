package org.specs2
package guide
package matchers

object OutsideSpecs2 extends UserGuidePage { def is = s2"""

## Outside specs2

The $specs2 matchers are a well-delimited piece of functionality that you should be able to reuse in your own test framework. You can reuse the following traits:

 * `${fullName[matcher.MustMatchers]}` (or `${fullName[matcher.ShouldMatchers]}`) to write anything like `1 must be_==(1)` and
   get a `Result` back

 * You can also use the side-effecting version of that trait called `${fullName[matcher.MustThrownMatchers]}` (or `${fullName[matcher.ShouldThrownMatchers]}`).
   It throws a `FailureException` as soon as an expectation is failing. Those traits can also be used in a regular specification if you have several expectations per example and if you don't want to chain them with `and`.

 * Finally, in a JUnit-like library you can use the `org.specs2.matcher.JUnitMustMatchers` trait which throws `AssertionFailureError`s

### Without any dependency on specs2

The [Testing](https://github.com/spray/spray/wiki/Testing) page of the ***spray*** project explains how you can define a testing trait in your library which can be used with $specs2 or scalatest or any framework defining the following methods:

   * `fail(msg: String): Nothing`
   * `skip(msg: String): Nothing`

In specs2, those 2 methods are defined by the `${fullName[matcher.ThrownMessages]}` trait
```
trait ThrownMessages { this: ThrownExpectations =>
  def fail(m: String): Nothing = failure(m)
  def skip(m: String): Nothing = skipped(m)
}
```
"""
}
