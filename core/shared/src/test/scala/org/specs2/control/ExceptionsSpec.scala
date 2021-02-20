package org.specs2
package control

import sys.*

class ExceptionsSpec extends Spec with Exceptions {  def is = s2"""

The Exceptions trait provides functional ways to catch exceptions and deal with them:

  * tryo returns the result in an Option
  * trye returns the result in an Either with the Left value being a function of the exception
  * catchAll is similar to trye but even catches Throwables
  * tryOr returns the result or a value built from the thrown exception
  * catchAllOr is similar to tryOr but even catches Throwables
  * tryOrElse returns the result or a default value
  * tryMap returns different values depending on the success of the expression
  * tryOk returns true iff the expression doesn't throw an Exception

  tryo executes an expression and returns an Option
    Some(result) if the expression doesn't throw an exception                            $tryo1
    None if the expression failed                                                        $tryo2

  tryOr executes an expression and returns the result or a default value. It returns
    the result if the expression doesn't throw an exception                              $tryOr1
    a default value if the expression throws an exception                                $tryOr2

  tryOrElse executes an expression and returns the result or another value. It returns
    the result if the expression doesn't throw an exception                              $tryOrElse1
    another value if the expression throws an exception                                  $tryOrElse2

  tryMap executes an expression and returns
    a 'ok' value if the expression doesn't throw an exception                            $tryMap1
    a 'ko' value if the expression throws an exception                                   $tryMap2

  tryOk executes an expression and returns a Boolean
    true if the expression doesn't throw an exception                                    $tryOk1
    false if the expression throws an exception                                          $tryOk2

  trye executes an expression and returns an Either value
    + Right(result) if the expression doesn't throw an exception                         $trye1
    + Left(f(e)) if the expression failed, where f is a function of an exception         $trye2

  catchAll executes an expression and returns an Either value
    Right(result) if the expression doesn't throw a Throwable                            $catchAll1
    Left(f(e)) if the expression threw anything, even an error                           $catchAll2

  catchAllOr executes an expression and returns the result or a default value. It returns
    the result if the expression doesn't throw an exception                              $catchAllOr1
    a default value if the expression throws an exception                                $catchAllOr2

  tryCollect uses a partial function to evaluate a value
    tryCollect returns a boolean                                                         $tryCollect1
    tryCollectOr returns any value                                                       $tryCollect2

 """

  def tryo1 = tryo("a") `must` ===(Some("a"))
  def tryo2 = tryo(boom) `must` ===(None)

  def tryOr1 = tryOr("a")(_.getMessage) `must` ===("a")
  def tryOr2 = tryOr(boom)(_ => "bang") `must` ===("bang")

  def tryOrElse1 = tryOrElse("a")("b") `must` ===("a")
  def tryOrElse2 = tryOrElse(boom)("bang") `must` ===("bang")

  def tryMap1 = tryMap("a")(true)(false) `must` ===(true)
  def tryMap2 = tryMap(boom)(true)(false) `must` ===(false)

  def tryOk1 = tryOk("a") `must` ===(true)
  def tryOk2 = tryOk(boom) `must` ===(false)

  def trye1 = trye("a")(_.getMessage) `must` ===(Right("a"))
  def trye2 = trye(boom)(_.getMessage) `must` ===(Left("boom"))

  def catchAll1 = catchAll("a")(_.getMessage) `must` ===(Right("a"))
  def catchAll2 = catchAll({throw new Error("boom"); "a"})(_.getMessage) `must` ===(Left("boom"))

  def catchAllOr1 = catchAllOr("a")(_.getMessage) `must` ===("a")
  def catchAllOr2 = catchAllOr({throw new Error("boom"); "a"})(_ => "bang") `must` ===("bang")

  def tryCollect1 = tryCollect("a") { case x => x == "a" }
  def tryCollect2 = tryCollectOr("x", 100) { case x => x.toInt } `must` ===(100)

  def boom = { error("boom"); "a" }

}
