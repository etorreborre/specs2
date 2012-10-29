package org.specs2
package control

import sys._
import specification.Grouped

class ExceptionsSpec extends Specification with Exceptions with Grouped {  def is =
                                                                                                                        """
The Exceptions trait provides functional ways to catch exceptions and deal with them:

  * tryo returns the result in an Option
  * trye returns the result in an Either with the Left value being a function of the exception
  * catchAll is similar to trye but even catches Throwables
  * tryOr returns the result or a value built from the thrown exception
  * catchAllOr is similar to tryOr but even catches Throwables
  * tryOrElse returns the result or a default value
  * tryMap returns different values depending on the success of the expression
  * tryOk returns true iff the expression doesn't throw an Exception
                                                                                                                        """^
                                                                                                                        p^
  "tryo executes an expression and returns an Option"                                                                   ^
    "Some(result) if the expression doesn't throw an exception"                                                         ! g1.e1^
    "None if the expression failed"                                                                                     ! g1.e2^
                                                                                                                        p^
  "tryOr executes an expression and returns the result or a default value. It returns"                                  ^
    "the result if the expression doesn't throw an exception"                                                           ! g2.e1^
    "a default value if the expression throws an exception"                                                             ! g2.e2^
                                                                                                                        p^
  "tryOrElse executes an expression and returns the result or another value. It returns"                                ^
    "the result if the expression doesn't throw an exception"                                                           ! g3.e1^
    "another value if the expression throws an exception"                                                               ! g3.e2^
                                                                                                                        p^
  "tryMap executes an expression and returns"                                                                           ^
    "a 'ok' value if the expression doesn't throw an exception"                                                         ! g4.e1^
    "a 'ko' value if the expression throws an exception"                                                                ! g4.e2^
                                                                                                                        p^
  "tryOk executes an expression and returns a Boolean"                                                                  ^
    "true if the expression doesn't throw an exception"                                                                 ! g5.e1^
    "false if the expression throws an exception"                                                                       ! g5.e2^
                                                                                                                        p^
  "trye executes an expression and returns an Either value"                                                             ^
    "Right(result) if the expression doesn't throw an exception"                                                        ! g6.e1^
    "Left(f(e)) if the expression failed, where f is a function of an exception"                                        ! g6.e2^
                                                                                                                        p^
  "catchAll executes an expression and returns an Either value"                                                         ^
    "Right(result) if the expression doesn't throw a Throwable"                                                         ! g7.e1^
    "Left(f(e)) if the expression threw anything, even an error"                                                        ! g7.e2^
                                                                                                                        p^
  "tryOr executes an expression and returns the result or a default value. It returns"                                  ^
    "the result if the expression doesn't throw an exception"                                                           ! g8.e1^
    "a default value if the expression throws an exception"                                                             ! g8.e2^
                                                                                                                        end
    
  "tryo" - new g1 {
    e1 = tryo("a") must_== Some("a")
    e2 = tryo(boom) must_== None
  }
  "tryOr" - new g2 {
    e1 = tryOr("a")((e:Exception) => e.getMessage) must_== "a"
    e2 = tryOr(boom)((e:Exception) => "bang") must_== "bang"
  }
  "tryOrElse" - new g3 {
    e1 = tryOrElse("a")("b") must_== "a"
    e2 = tryOrElse(boom)("bang") must_== "bang"
  }
  "tryMap" - new g4 {
    e1 = tryMap("a")(true)(false) must_== true
    e2 = tryMap(boom)(true)(false) must_== false
  }
  "tryOk" - new g5 {
    e1 = tryOk("a") must_== true
    e2 = tryOk(boom) must_== false
  }
  "trye" - new g6 {
    e1 = trye("a")((e:Exception) => e.getMessage) must_== Right("a")
    e2 = trye(boom)((e:Exception) => e.getMessage) must_== Left("boom")
  }
  "catchAll" - new g7 {
    e1 = catchAll("a")((e:Throwable) => e.getMessage) must_== Right("a")
    e2 = catchAll({throw new Error("boom");"a"})((e:Throwable) => e.getMessage) must_== Left("boom")
  }
  "catchAllOr" - new g8 {
    e1 = catchAllOr("a")((e:Throwable) => e.getMessage) must_== "a"
    e2 = catchAllOr({throw new Error("boom");"a"})((e:Throwable) => "bang") must_== "bang"
  }

  def boom = { error("boom"); "a" }

}