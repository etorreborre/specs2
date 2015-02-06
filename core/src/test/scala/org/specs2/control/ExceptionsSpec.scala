package org.specs2
package control

import sys._
import specification._

class ExceptionsSpec extends script.Spec with Exceptions with Grouped {  def is = s2"""
                                                                                                               
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
    + Some(result) if the expression doesn't throw an exception
    + None if the expression failed

  tryOr executes an expression and returns the result or a default value. It returns
    + the result if the expression doesn't throw an exception
    + a default value if the expression throws an exception

  tryOrElse executes an expression and returns the result or another value. It returns
    + the result if the expression doesn't throw an exception
    + another value if the expression throws an exception

  tryMap executes an expression and returns
    + a 'ok' value if the expression doesn't throw an exception
    + a 'ko' value if the expression throws an exception

  tryOk executes an expression and returns a Boolean
    + true if the expression doesn't throw an exception
    + false if the expression throws an exception

  trye executes an expression and returns an Either value
    + Right(result) if the expression doesn't throw an exception
    + Left(f(e)) if the expression failed, where f is a function of an exception

  catchAll executes an expression and returns an Either value
    + Right(result) if the expression doesn't throw a Throwable
    + Left(f(e)) if the expression threw anything, even an error

  tryOr executes an expression and returns the result or a default value. It returns
    + the result if the expression doesn't throw an exception
    + a default value if the expression throws an exception

  tryCollect uses a partial function to evaluate a value
    + tryCollect returns a boolean
    + tryCollectOr returns any value
                                                                                                  """
    
  "tryo" - new group {
    eg := tryo("a") must_== Some("a")
    eg := tryo(boom) must_== None

    eg := tryOr("a")((e:Exception) => e.getMessage) must_== "a"
    eg := tryOr(boom)((e:Exception) => "bang") must_== "bang"

    eg := tryOrElse("a")("b") must_== "a"
    eg := tryOrElse(boom)("bang") must_== "bang"

    eg := tryMap("a")(true)(false) must_== true
    eg := tryMap(boom)(true)(false) must_== false

    eg := tryOk("a") must_== true
    eg := tryOk(boom) must_== false

    eg := trye("a")((e:Exception) => e.getMessage) must_== Right("a")
    eg := trye(boom)((e:Exception) => e.getMessage) must_== Left("boom")

    eg := catchAll("a")((e:Throwable) => e.getMessage) must_== Right("a")
    eg := catchAll({throw new Error("boom"); "a"})((e:Throwable) => e.getMessage) must_== Left("boom")

    eg := catchAllOr("a")((e:Throwable) => e.getMessage) must_== "a"
    eg := catchAllOr({throw new Error("boom"); "a"})((e:Throwable) => "bang") must_== "bang"

    eg := tryCollect("a") { case x => x == "a" }
    eg := tryCollectOr("x", 100) { case x => x.toInt } must_== 100

  }

  def boom = { error("boom"); "a" }

}