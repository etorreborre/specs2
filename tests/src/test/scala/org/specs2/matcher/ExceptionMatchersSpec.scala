package org.specs2
package matcher

import scala.util.control.NonFatal
import sys.*
import execute.AsResult
import text.Regexes.*

class ExceptionMatchersSpec extends Specification with ResultMatchers { def is = s2"""

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
  it must return an Error if B is thrown $byType7
  it must return a Failure if A is thrown $byType8
  it must return a Success if no exception $byType9

 With a PartialFunction
 ======================

 it is also possible to specify that the thrown exception is ok according to a PartialFunction
   'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'b') $pf1
   'error(boom) must throwA[RuntimeException].like(e => e.getMessage(0) === 'a') will fail $pf2

 negated matcher: when we specify not(throw[A].like(f))
  it must return an Error if B is thrown $pf3
  it must return a Failure if A is thrown like f $pf4
  it must return an Error if A is thrown not like f $pf5
  it must return a Success if no exception like f $pf6

 With a regular expression
 =========================

 more simply the exception message can be specified with a regular expression
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
   failing if the catched expression doesn't satisfy the partial function $specific5
   succeeding otherwise $specific6

 With combinators
 ================

  negating a throw matcher must return the proper success message $combinators1

 Stacktrace
 ==========

 the stacktrace of the caught exception must be displayed $stacktrace1

"""

  type IAE = IllegalArgumentException

  def byType1 = ("hello" `must` throwAn[Error]).message `must` ===("Expected: java.lang.Error. Got nothing")

  def byType2 = (theBlock(error("boom")) `must` throwA[RuntimeException]).message `must` ===(
    "Got the exception java.lang.RuntimeException: boom")

  def byType3 = (theBlock(error("boom")) `must` throwAn[IAE]).message `must` startWith(
    "Expected: java.lang.IllegalArgumentException. Got: java.lang.RuntimeException: boom instead")

  def byType4 = (1 `must` not(throwA(new Exception))) `must` beSuccessful

  def byType5 = ({sys.error("boom"); 1} `must` not(throwAn[Exception])) `must` beFailing

  def byType6 = { throw new StackOverflowError("play again"); 1 } `must` throwAn[Error]

  def byType7 = AsResult {{throw new NullPointerException; 1 } `must` not(throwAn[IAE]) } `must` beError

  def byType8 = AsResult {{throw new IAE; 1 } `must` not(throwAn[IAE]) } `must` beFailing

  def byType9 = AsResult { 1  `must` not(throwAn[IAE]) } `must` beSuccessful

  def pf1 = (theBlock(error("boom")) `must` throwA[RuntimeException].like { case NonFatal(e) =>
    e.getMessage()(0) === 'b'
  }).message `must` startWith(
      "Got the exception java.lang.RuntimeException: boom and b == 'b'")
  // todo: figure how to quote only the value
  //      "Got the exception java.lang.RuntimeException: boom and 'b' == 'b'")

  def pf2 = (theBlock(error("boom")) `must` throwA[RuntimeException].like { case NonFatal(e) => e.getMessage()(0) === 'a' }).message `must` startWith(
    "Expected: java.lang.RuntimeException. Got: java.lang.RuntimeException: boom and b != a")

  def pf3 = AsResult { {throw new NullPointerException; 1 } `must` not(throwAn[IAE].like { case _ => ok }) } `must` beError

  def pf4 = AsResult { {throw new IAE; 1 } `must` not(throwAn[IAE].like { case _ => ok }) } `must` beFailing

  def pf5 = AsResult { {throw new IAE; 1 } `must` not(throwAn[IAE].like { case _ => ko }) } `must` beError

  def pf6 = AsResult { 1  `must` not(throwAn[IAE].like { case _ => ok }) } `must` beSuccessful

  def regex1 = (theBlock(error("boom")) `must` throwA[RuntimeException](message = "boo")).message `must` startWith(
    s"Got the exception java.lang.RuntimeException: boom and boom matches '${"boo".regexPart}'")
    // todo: figure how to quote only the value
    //      s"Got the exception java.lang.RuntimeException: boom and 'boom' matches '${"boo".regexPart}'")

  def regex2 = (theBlock(error("boom\nbang\nbong")) `must` throwA[RuntimeException](message = "bang")).message `must` startWith(
      s"Got the exception java.lang.RuntimeException: boom\nbang\nbong and boom\nbang\nbong matches '${"bang".regexPart}'")
    // todo: figure how to quote only the value
    //      s"Got the exception java.lang.RuntimeException: boom\nbang\nbong and 'boom\nbang\nbong' matches '${"bang".regexPart}'")

  def regex3 = (theBlock(error("bang\nboom\nbong")) `must` throwA[RuntimeException](message = "bang")).message `must` startWith(
      s"Got the exception java.lang.RuntimeException: bang\nboom\nbong and bang\nboom\nbong matches '${"bang".regexPart}'")
    // todo: figure how to quote only the value
    //      s"Got the exception java.lang.RuntimeException: bang\nboom\nbong and 'bang\nboom\nbong' matches '${"bang".regexPart}'")

  def specific1 = ("hello" `must` throwA(new RuntimeException("boom"))).message `must` ===(
    "Expected: java.lang.RuntimeException: boom. Got nothing")

  def specific2 = (theBlock(error("boom")) `must` throwAn(new RuntimeException("boom"))).message `must` startWith(
    "Got the exception java.lang.RuntimeException: boom")

  def specific3 = (theBlock(error("boom")) `must` throwAn(new IAE("boom"))).message `must` startWith(
    "Expected: java.lang.IllegalArgumentException: boom. Got: java.lang.RuntimeException: boom instead")

  def specific4 = (theBlock(error("boom")) `must` throwAn(new RuntimeException("bang"))).message `must` startWith(
    "Expected: java.lang.RuntimeException: bang. Got: java.lang.RuntimeException: boom instead")

  case class UserError(name: String, message: String) extends RuntimeException(message)

  def specific5 = (theBlock(throw UserError("me", "boom")) `must` throwAn(UserError("me2", "boom")).
    like { case UserError(name, _) => name `must` endWith("2") }).message `must` contain("me doesn't end with '2'")
    // todo: figure how to quote only the value
    //      like { case UserError(name, _) => name must endWith("2") }).message must contain("'me' doesn't end with '2'")

  def specific6 = (theBlock(throw UserError("me", "boom")) `must` throwAn(UserError("me2", "boom")).
    like { case UserError(name, _) => name `must` startWith("m") }).message `must` beMatching("Got the exception .*")

  def combinators1 = (1 `must` not(throwAn[Exception])).message `must` ===("Expected: java.lang.Exception. Got nothing")

  def stacktrace1= (theBlock(error("boom")) `must` throwAn[IllegalArgumentException]).message `must` contain(
    "The  RuntimeException stacktrace is")
}
