package org.specs2
package matcher

import scala.util.control.NonFatal
import sys.*
import execute.AsResult
import text.Regexes.*

class ExceptionMatchersSpec extends Specification with ResultMatchers:
  def is = s2"""

 Exception matchers allow to check that exceptions are thrown

 By type
 =======

 by specifying the expected type of exception: 'value must throwAn[Exception]'
  it must fail if the exception is not thrown $byType1
  it must succeed if the exception is thrown with the expected type $byType2
  it must fail if the exception is thrown with the wrong type $byType3
  it must not fail if the exception is not thrown and the matcher is negated $byType4
  it must not fail when the exception type is not specified $byType5
  it must return an Error when an Exception is expected and a java.lang.Error is thrown $byType6

 negated matcher: when we specify not(throw[A])
  it must succeed if no exception is thrown $byType7
  it must succeed if an exception is thrown with a different type $byType8
  it must fail if A is thrown $byType9
  it must fail if a subtype of A is thrown $byType10
  it must return the exception stacktrace in case of a failure $byType11

 With a PartialFunction
 ======================

 it is also possible to specify that the thrown exception is ok according to a PartialFunction
   'error(boom) must throwA[RuntimeException].like(e => e.getMessage must startWith("b")) succeeds $pf1
   'error(boom) must throwA[RuntimeException].like(e => e.getMessage must startWith("a")) fails $pf2

 negated matcher: when we specify not(throw[A].like(f))
  it must succeed if no exception is thrown $pf3
  it must succeed if an exception is thrown with a different type $pf4
  it must succeed if an exception is thrown with with the right type but not satisfying the partial function $pf4
  it must fail if A is thrown and satisfies the partial function $pf6
  it must return the exception stacktrace in case of a failure $pf7

 With a regular expression
 =========================

 The exception message can be specified with a regular expression
  'error(boom) must throwA[RuntimeException](message = 'boo') $regex1
  for a multi-line string $regex2
  for match which is followed by multiple lines $regex3

 With a specific exception
 =========================

 by specifying the expected exception: 'value must throwA(new java.lang.RuntimeException('wrong')'
  it must fail if the exception is not thrown at all $specific1
  it must succeed if an exception of same class and message is thrown $specific2
  it must fail if an exception of a different class and same message is thrown $specific3
  it must fail if an exception of a same class and different message is thrown $specific4
  it can be refined with a 'like' expression
   failing if the caught expression doesn't satisfy the partial function $specific5
   succeeding otherwise $specific6

 An exception value
 ==================

 A Throwable can be checked for its class and message $throwable1

"""

  type IAE = IllegalArgumentException

  def byType1 = ("hello" must throwAn[Error]).message must ===("No exception of type java.lang.Error was thrown")

  def byType2 = theBlock(error("boom")) must throwA[RuntimeException]

  def byType3 = (theBlock(error("boom")) must throwAn[IAE]).message must startWith(
    "Caught an exception of type java.lang.RuntimeException which is not of type java.lang.IllegalArgumentException: boom"
  )

  def byType4 = (1 must not(throwA(new Exception))) must beSuccessful

  def byType5 = ({ sys.error("boom"); 1 } must not(throwAn[Exception])) must beFailing

  def byType6 = ({ throw new StackOverflowError("play again"); 1 } must throwAn[Exception]) must throwA[java.lang.Error]

  def byType7 = (1 must not(throwAn[Exception])).message must ===("No exception of type java.lang.Exception was thrown")

  def byType8 = {
    val result = theBlock(error("boom")) must not(throwAn[IllegalArgumentException])
    result and (result.message must contain(
      "Caught an exception of type java.lang.RuntimeException which is not of type java.lang.IllegalArgumentException: boom"
    ))
  }

  def byType9 = (theBlock(error("boom")) must not(throwAn[Exception])) must beFailing

  def byType10 =
    (theBlock(throw new IllegalArgumentException("boom")) must not(throwA[RuntimeException])) must beFailing

  def byType11 = (theBlock(error("boom")) must not(throwAn[Exception])).message must contain("The stacktrace is")

  def pf1 = (theBlock(error("boom")) must throwA[RuntimeException].like { case NonFatal(e) =>
    e.getMessage must startWith("b")
  })

  def pf2 = {
    val result = theBlock(error("boom")) must throwA[RuntimeException].like { case NonFatal(e) =>
      e.getMessage must startWith("a")
    }
    (result must beFailing) and
      (result.message must (contain("Caught an exception of type java.lang.RuntimeException: boom")
        and contain(
          "the exception does not satisfy the specified condition: boom doesn't start with 'a'"
        )))
  }

  // Partial functions

  def pf3 = (1 must not(throwAn[Exception].like { case _ => ok })).message must ===(
    "No exception of type java.lang.Exception was thrown"
  )

  def pf4 = {
    val result = theBlock(error("boom")) must not(throwAn[IllegalArgumentException].like { case _ => ok })
    result and (result.message must contain(
      "Caught an exception of type java.lang.RuntimeException which is not of type java.lang.IllegalArgumentException: boom"
    ))
  }

  def pf5 = (1 must not(throwAn[Exception].like { case _ => ko })).message must contain(
    "Caught an exception of type java.lang.RuntimeException which is not of type java.lang.IllegalArgumentException: boom"
  )

  def pf6 = {
    val result = theBlock(error("boom")) must not(throwA[RuntimeException].like { case NonFatal(e) =>
      e.getMessage must startWith("b")
    })
    (result must beFailing) and
      (result.message must (contain("Caught an exception of type java.lang.RuntimeException: boom")
        and contain(
          "The exception satisfies the specified condition: boom doesn't start with 'b'"
        )))
  }

  def pf7 = {
    val result = theBlock(error("boom")) must not(throwA[RuntimeException].like { case NonFatal(e) =>
      e.getMessage must startWith("b")
    })
    result.message must contain("The stacktrace is")
  }

  def regex1 = (theBlock(error("boom")) must throwA[RuntimeException](message = "boo"))

  def regex2 = (theBlock(error("boom\nbang\nbong")) must throwA[RuntimeException](message = "bang"))

  def regex3 = (theBlock(error("bang\nboom\nbong")) must throwA[RuntimeException](message = "bang"))

  def specific1 = ("hello" must throwA(new RuntimeException("boom"))).message must ===(
    "No exception of type java.lang.RuntimeException was thrown"
  )

  def specific2 = (theBlock(error("boom")) must throwAn(new RuntimeException("boom")))

  def specific3 = (theBlock(error("boom")) must throwAn(new IAE("boom"))).message must startWith(
    "Caught an exception of type java.lang.RuntimeException which is not of type java.lang.IllegalArgumentException: boom"
  )

  def specific4 = {
    val result = (theBlock(error("boom")) must throwAn(new RuntimeException("bang"))).message
    (result must startWith("Caught an exception of type java.lang.RuntimeException: boom")) and
      (result must contain("the exception does not satisfy the specified condition: 'boom' != 'bang'"))
  }

  case class UserError(name: String, message: String) extends RuntimeException(message)

  def specific5 = (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).like {
    case UserError(name, _) => name must endWith("2")
  }).message must contain(
    "the exception does not satisfy the specified condition: me doesn't end with '2'"
  )

  def specific6 = (theBlock(throw UserError("me", "boom")) must throwAn(UserError("me2", "boom")).like {
    case UserError(name, _) => name must startWith("m")
  })

  def throwable1 =
    (new IllegalArgumentException("incorrect arguments"): Throwable) must beException[IllegalArgumentException](
      ".*arguments.*"
    )
