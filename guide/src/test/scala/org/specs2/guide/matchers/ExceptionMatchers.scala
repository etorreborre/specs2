package org.specs2
package guide
package matchers

object ExceptionMatchers extends UserGuideCard {
  def title = "Exception"
  def text = s2"""
 $specs2 offers very compact ways of checking that some exceptions are thrown:

### for expressions throwing an exception

  * `throwA[ExceptionType]` checks if a block of code throws an exception of the given type
  * `throwA[ExceptionType](message = "boom")` additionally checks if the exception message is as expected (`message` is
     being interpreted as a regular expression)
  * `throwA(exception)` or `throwAn(exception)` checks if a block of code throws an exception of the same type, with the
  same message
  * `throwA[ExceptionType].like { case e => e must matchSomething }` or
  `throwA(exception).like { case e => e must matchSomething }` checks that the thrown exception satisfies a property
  * `throwA[ExceptionType](me.like { case e => e must matchSomething }` or
  `throwA(exception).like { case e => e must matchSomething }` checks that the thrown exception satisfies a property

### for expressions that shouldn't throw an exception
  * `notThrow` checks that an expression doesn't throw any exceptions

### for exception values

  * `beException[ExceptionType]("message")` checks that a `Throwable` has an expected type and that its message satisfies
  a regular expression

----
For all the above matchers you can use `throwAn` instead of `throwA` if the exception name starts with a vowel for better
readability.
"""
}
