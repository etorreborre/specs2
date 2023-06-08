package org.specs2
package matcher

import util.{Success as Succeeded, Failure as Failed}

// format: off
class TryMatchersSpec extends Spec with TryMatchers with ResultMatchers {
  def is = s2"""

 The TryMatchers trait provides matchers to check Try instances.

  beSuccessful checks if an element is Success(_)
  ${Succeeded(1) must beSuccessfulTry}
  ${Succeeded(1) must beSuccessfulTry(1)}
  ${Succeeded(1) must beSuccessfulTry.withValue(1)}
  ${Succeeded(1) must beSuccessfulTry.withValue(===(1))}
  ${Succeeded(1) must beASuccessfulTry.which(_ > 0)}
  ${(Succeeded(1) must beASuccessfulTry.which(_ > 0))}
  ${(Succeeded(1) must beASuccessfulTry
    .which(_ < 0)) returns "Success(1) is a Success but the function returns 'false' on '1'"}
  ${(Failed[I](e) must beASuccessfulTry.which(_ > 0)) returns "Failure(boom) is not a Success\n\nFailed with boom:\n\n"}

  ${Succeeded(1) must beASuccessfulTry[Int].like { case a if a > 0 => ok }}
  ${(Succeeded(1) must not(beASuccessfulTry[Int].like { case a => a must be_>=(0) })) returns "Expectation failed: 'Success(1) is a Success but 1 is strictly less than 0'"}
  ${Succeeded(1) must not(beASuccessfulTry.withValue(2))}
  ${Failed[I](e) must not(beASuccessfulTry)}
  ${Failed[I](e) must not(beASuccessfulTry.withValue(2))}
  ${(Failed[I](e) must beSuccessfulTry) returns "Failure(boom) is not a Success\n\nFailed with boom:\n\n"}
  ${(Succeeded(1) must beSuccessfulTry.withValue(2)) returns "Success(1) is a Success but 1 != 2"}

  beAFailure checks if an element is Failure(_)
  ${Failed[I](e) must beFailedTry(e)}
  ${Failed[I](e) must beFailedTry}
  ${Succeeded(1) must not(beAFailedTry)}
  ${(Succeeded(1) must beFailedTry) returns "Success(1) is not a Failure"}
  ${Failed[I](e) must beFailedTry}
  ${Failed[I](e) must beFailedTry.withThrowable[MyException]}
  ${Failed[I](e) must beFailedTry.withThrowable[Exception]}
  ${(Failed[I](e) must beFailedTry.withThrowable[OtherException]) returns
    s"Failure(boom) is a Failure but '$e: ${classOf[MyException].getName}' is not an instance of '${classOf[OtherException].getName}'"}
  ${Failed[I](e) must beAFailedTry.withThrowable[MyException](".*oo.*")}
  ${Failed[I](e) must beFailedTry.like { case e: MyException => e.getMessage must startWith("b") }}
  ${Failed[I](e) must not(beAFailedTry.withThrowable[MyException]("bang"))}
  ${(Failed[I](e) must beAFailedTry
    .withThrowable[MyException]("bang")) returns "Failure(boom) is a Failure but 'boom' doesn't match 'bang'"}
  ${Failed[I](e) must not(beAFailedTry.withThrowable[OtherException])}
  ${Failed[I](ExceptionTrait("bang")) must beFailedTry.withThrowable[ExceptionTrait]}
  ${Failed[I](ExceptionTrait("bang")) must beFailedTry.withThrowable[ExceptionTrait]("bang")}
  """

  val e = new MyException("boom")
  class MyException(m: String) extends Exception(m):
    override def toString = m
    override def equals(o: Any) = o.asInstanceOf[Matchable] match
      case e: MyException => e.getMessage == getMessage
      case _              => false
  class OtherException extends Exception
  type I = Int

  trait ExceptionTrait extends Exception
  object ExceptionTrait:
    def apply(message: String): ExceptionTrait = new ExceptionTrait {
      override def getMessage: String = message
    }
}
