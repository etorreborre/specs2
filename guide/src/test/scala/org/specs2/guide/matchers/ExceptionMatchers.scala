package org.specs2
package guide
package matchers

import form.Card

object ExceptionMatchers extends Card {
  def title = "Exception"
  def text = s2"""
***specs2*** offers very compact ways of checking that some exceptions are thrown:

 * `throwA[ExceptionType]` check if a block of code throws an exception of the given type
 * `throwA[ExceptionType](message = "boom")` additionally check if the exception message is as expected
 * `throwA(exception)` or `throwAn(exception)` check if a block of code throws an exception of the same type, with the
 same message
 * `throwA[ExceptionType].like { case e => e must matchSomething }` or
 `throwA(exception).like { case e => e must matchSomething }` check that the thrown exception satisfies a property
 * `throwA[ExceptionType](me.like { case e => e must matchSomething }` or
 `throwA(exception).like { case e => e must matchSomething }` check that the thrown exception satisfies a property

For all the above matchers you can use `throwAn` instead of `throwA` if the exception name starts with a vowel for better
readability.
"""
}
