package org.specs2
package matcher

import util.{Success => Succeeded, Failure => Failed}

class TryMatchersSpec extends Spec with TryMatchers with ResultMatchers { def is = s2"""

 The TryMatchers trait provides matchers to check Try instances.

  beSuccessful checks if an element is Success(_)
  ${ Succeeded(1) must beSuccessfulTry }
  ${ Succeeded(1) must beSuccessfulTry(1) }
  ${ Succeeded(1) must beSuccessfulTry.withValue(1) }
  ${ Succeeded(1) must beSuccessfulTry.withValue(===(1)) }
  ${ Succeeded(1) must beASuccessfulTry.which(_ > 0) }
  ${ (Succeeded(1) must beASuccessfulTry.which(_ > 0)) returns "Success(1) is a Success and the function returns 'true' on '1'" }
  ${ (Succeeded(1) must beASuccessfulTry.which(_ < 0)) returns "Success(1) is a Success but the function returns 'false' on '1'" }
  ${ (Failed[I](e) must beASuccessfulTry.which(_ > 0)) returns "Failure(boom) is not a Success" }

  ${ Succeeded(1) must beASuccessfulTry.like { case a if a > 0 => ok } }
  ${ (Succeeded(1) must not(beASuccessfulTry.like { case a => a must be_>=(0) })) returns "Success(1) is a Success and 1 is not less than 0" }
  ${ Succeeded(1) must not be aSuccessfulTry.withValue(2) }
  ${ Failed[I](e) must not be successfulTry }
  ${ Failed[I](e) must not be successfulTry.withValue(2) }
  ${ (Failed[I](e) must beSuccessfulTry) returns "Failure(boom) is not a Success" }
  ${ (Succeeded(1) must beSuccessfulTry.withValue(2)) returns "Success(1) is a Success but 1 != 2" }

  beAFailure checks if an element is Failure(_)
  ${ Failed[I](e) must beFailedTry(e) }
  ${ Failed[I](e) must beFailedTry }
  ${ Succeeded(1) must not be failedTry }
  ${ (Succeeded(1) must be failedTry) returns "Success(1) is not a Failure" }
  ${ Failed[I](e) must be aFailedTry }
  ${ Failed[I](e) must beFailedTry.withThrowable[MyException] }
  ${ Failed[I](e) must beFailedTry.withThrowable[Exception] }
  ${ (Failed[I](e) must beFailedTry.withThrowable[OtherException]) returns
     s"Failure(boom) is a Failure but '$e: ${classOf[MyException].getName}' is not an instance of '${classOf[OtherException].getName}'" }
  ${ Failed[I](e) must beAFailedTry.withThrowable[MyException](".*oo.*") }
  ${ Failed[I](e) must beFailedTry.like { case e: MyException => e.getMessage must startWith("b") }}
  ${ Failed[I](e) must not be aFailedTry.withThrowable[MyException]("bang") }
  ${ (Failed[I](e) must beAFailedTry.withThrowable[MyException]("bang")) returns "Failure(boom) is a Failure but 'boom' doesn't match 'bang'" }
  ${ Failed[I](e) must not be aFailedTry.withThrowable[OtherException] }

  """

  val e = new MyException("boom")
  class MyException(m: String) extends Exception(m) {
    override def toString = m
    override def equals(o: Any) = o match {
      case e: MyException => e.getMessage == getMessage
      case _ => false
    }
  }
  class OtherException extends Exception
  type I = Int
}
