package org.specs2
package matcher

import util.{Success => Succeeded, Failure => Failed}

class TryMatchersSpec extends Specification { def is = s2"""

 The TryMatchers trait provides matchers to check Try instances.

  beSuccessful checks if an element is Success(_)
  ${ Succeeded(1) must beSuccessfulTry }
  ${ Succeeded(1) must beSuccessfulTry.withValue(1) }
  ${ Succeeded(1) must beASuccessfulTry.which(_ > 0) }
  ${ Succeeded(1) must beASuccessfulTry.like { case a if a > 0 => ok } }
  ${ Succeeded(1) must not be aSuccessfulTry.withValue(2) }
  ${ Failed[I](e) must not be successfulTry }
  ${ Failed[I](e) must not be successfulTry.withValue(2) }

  beAFailure checks if an element is Failure(_)
  ${ Failed[I](e) must beFailedTry }
  ${ Succeeded(1) must not be failedTry }
  ${ Failed[I](e) must be aFailedTry }
  ${ Failed[I](e) must beFailedTry.withThrowable[MyException] }
  ${ Failed[I](e) must beAFailedTry.withThrowable[MyException](".*oo.*") }
  ${ Failed[I](e) must not be aFailedTry.withThrowable[MyException]("bang") }
  ${ Failed[I](e) must not be aFailedTry.withThrowable[OtherException] }

  """

  def e = new MyException("boom")
  class MyException(m: String) extends Exception(m)
  class OtherException extends Exception
  type I = Int
}
